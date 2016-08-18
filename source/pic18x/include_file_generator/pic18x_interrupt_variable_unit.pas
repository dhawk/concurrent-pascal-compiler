UNIT pic18x_interrupt_variable_unit;

INTERFACE

uses
   pic18x_information_unit, Generics.Collections,
   common_unit;

type
   t_interrupt_variables =
      class
      private
         type
            t_possible_interrupt_info =
               class
                  enable_bit: string;
                  enable_bit_sfr: string;
                  flag_bit: string;
                  flag_bit_sfr: string;
                  flag_bit_access: string;  // 'n' or 'r' or '-'
                  priority_bit: string;
                  priority_bit_sfr: string
               end;
            t_possible_interrupt_dictionary = TObjectDictionary<String, t_possible_interrupt_info>;
      public
         constructor Create (pic_info: TPICInfo);
         procedure AppendIncludeFileSource (out: TOutStringProc);
         destructor Destroy;
            override;
      private
         possible_interrupts: t_possible_interrupt_dictionary;
      end;

IMPLEMENTATION

uses
  SysUtils, Generics.Defaults, RegularExpressions;

var
   usart_transmit_interrupt_regex: TRegEx;
   usart_receive_interrupt_regex: TRegEx;

constructor t_interrupt_variables.Create (pic_info: TPICInfo);

   procedure record_possible_sfr_interrupt_bits (sfr: TSFRDef);
      var
         m: TSFRMode;
         f: TSFRField;
         possible_interrupt_name: string;
         possible_interrupt_info: t_possible_interrupt_info;
      begin  // record_possible_sfr_interrupt_bits
         for m in sfr.modes do
            for f in m.fields do
               if (f.width = 1)
                  and
                  (Length(f.fieldname) > 1)
                  and
                  (CharInSet (f.fieldname[Length(f.fieldname)], ['E','F','P']))
               then
                  begin
                     possible_interrupt_name := Copy (f.fieldname, 1, Length(f.fieldname)-1);
                     if possible_interrupts.ContainsKey (possible_interrupt_name) then
                        possible_interrupt_info := possible_interrupts [possible_interrupt_name]
                     else
                        begin
                           possible_interrupt_info := t_possible_interrupt_info.Create;
                           possible_interrupts.Add (possible_interrupt_name, possible_interrupt_info)
                        end;
                     case f.fieldname[Length(f.fieldname)] of
                        'E': begin
                                possible_interrupt_info.enable_bit := f.fieldname;
                                possible_interrupt_info.enable_bit_sfr := sfr.name
                             end;
                        'F': begin
                                possible_interrupt_info.flag_bit := f.fieldname;
                                possible_interrupt_info.flag_bit_sfr := sfr.name;
                                possible_interrupt_info.flag_bit_access := f.bit_access
                             end;
                        'P': begin
                                possible_interrupt_info.priority_bit := f.fieldname;
                                possible_interrupt_info.priority_bit_sfr := sfr.name
                             end;
                     else
                        assert (false)
                     end
                  end
      end;   // record_possible_sfr_interrupt_bits

   procedure record_possible_muxd_sfr_interrupt_bits (msfr: TMuxdSFR);
      var
         sfr: TSFRDef;
      begin
         for sfr in msfr.sfrs do
            record_possible_sfr_interrupt_bits (sfr)
      end;

   procedure record_possible_joined_sfr_interrupt_bits (jsfr: TJoinedSFR);
      begin
         assert (jsfr.sfrL.kind = jsfr.sfrH.kind);
         case jsfr.sfrL.kind of
            simple_sfr:
               begin
                  assert (jsfr.sfrH.kind = simple_sfr);
                  record_possible_sfr_interrupt_bits (TSFRDef(jsfr.sfrL));
                  record_possible_sfr_interrupt_bits (TSFRDef(jsfr.sfrH))
               end;
            muxd_sfr:
               begin
                  assert (jsfr.sfrL.kind = muxd_sfr);
                  assert (jsfr.sfrH.kind = muxd_sfr);
                  assert (jsfr.sfrU = nil);  // haven't seen this so far...
                  record_possible_muxd_sfr_interrupt_bits (TMuxdSFR(jsfr.sfrL));
                  record_possible_muxd_sfr_interrupt_bits (TMuxdSFR(jsfr.sfrH))
               end;
         else
            assert (false)
         end
      end;

   var
      sfr: TSFR;
   begin  // t_interrupt_variables.Create
      possible_interrupts := t_possible_interrupt_dictionary.Create([doOwnsValues]);

      // find all SFR 1-bit fields whose names end in 'E', 'F' or 'P'
      for sfr in pic_info.sfrs do
         case sfr.kind of
            simple_sfr:
               record_possible_sfr_interrupt_bits (TSFRDef(sfr));
            joined_sfr:
               record_possible_joined_sfr_interrupt_bits (TJoinedSFR(sfr));
            muxd_sfr:
               record_possible_muxd_sfr_interrupt_bits (TMuxdSFR(sfr))
         else
            assert (false)
         end
   end;   // t_interrupt_variables.Create

procedure t_interrupt_variables.AppendIncludeFileSource (out: TOutStringProc);

   var
      intr: string;
      keyArray: TArray<string>;
      keyCollection: TObjectDictionary<String, t_possible_interrupt_info>.TKeyCollection;
   begin
      keyCollection := nil;
      try
         keyCollection := TObjectDictionary<String, t_possible_interrupt_info>.TKeyCollection.Create (possible_interrupts);

         // remove non-interrupts from possible_interrupts
         keyArray := keyCollection.ToArray;
         for intr in keyArray do
            with possible_interrupts[intr] do
               if (enable_bit = '') or (flag_bit = '') then
                  possible_interrupts.Remove (intr);
         possible_interrupts.TrimExcess;

         if possible_interrupts.Count > 0 then
            begin
               // output alphabetically
               keyArray := keyCollection.ToArray;
               TArray.Sort<string> (keyArray,
                                    TComparer<string>.Construct
                                       (function (const L, R: string): integer
                                           begin
                                              result := CompareText (L, R)
                                           end
                                       )
                                    );

               out ('var');
               for intr in keyArray do
                  with possible_interrupts[intr] do
                     begin
                        out ('   ' + intr + '_prio2_interrupt:');
                        out ('      interrupt priority 2;');
                        out ('         function signaled: boolean;');
                        out ('            begin');
                        out ('               if (' + enable_bit_sfr + '.' + enable_bit + ' = 1)');
                        out ('                  and');
                        out ('                  (' + flag_bit_sfr + '.' + flag_bit + ' = 1)');
                        out ('               then');
                        if flag_bit_access = 'r' then
                           out ('                     result := true')
                        else
                           begin
                              out ('                  begin');
                              out ('                     ' + flag_bit_sfr + '.' + flag_bit + ' := 0;');
                              out ('                     result := true');
                              out ('                  end')
                           end;
                        out ('            end;');
                        out ('      begin');
                        out ('         ' + enable_bit_sfr + '.' + enable_bit + ' := 1');
                        out ('      end;');
                        out ('');
                        if priority_bit <> '' then
                           begin
                              out ('   ' + intr + '_prio1_interrupt:');
                              out ('      interrupt priority 1;');
                              out ('         function signaled: boolean;');
                              out ('            begin');
                              out ('               if (' + enable_bit_sfr + '.' + enable_bit + ' = 1)');
                              out ('                  and');
                              out ('                  (' + flag_bit_sfr + '.' + flag_bit + ' = 1)');
                              out ('               then');
                              if flag_bit_access = 'r' then
                                 out ('                     result := true')
                              else
                                 begin
                                    out ('                  begin');
                                    out ('                     ' + flag_bit_sfr + '.' + flag_bit + ' := 0;');
                                    out ('                     result := true');
                                    out ('                  end')
                                 end;
                              out ('            end;');
                              out ('      begin');
                              out ('         ' + priority_bit_sfr + '.' + priority_bit + ' := 0;');
                              out ('         ' + enable_bit_sfr + '.' + enable_bit + ' := 1');
                              out ('      end;');
                              out ('')
                           end
                     end
            end
      finally
         keyCollection.Free
      end
   end;

destructor t_interrupt_variables.Destroy;
   begin
      possible_interrupts.Free;
      inherited
   end;

INITIALIZATION
   usart_transmit_interrupt_regex := TRegEx.Create ('^TX[0-9]*I$');
   usart_receive_interrupt_regex := TRegEx.Create ('^RC[0-9]*I$');

END.

