UNIT pic18x_16bit_timer_unit;

INTERFACe

uses
   pic18x_information_unit, common_unit;

type
   tTimer16List =
      class
         constructor Create (_pic_info: TPICInfo);
         procedure AppendIncludeFileSource (out: TOutStringProc);
         procedure AppendXMLFile (out: TOutStringProc);
      private
         timers: array of integer;
      end;

IMPLEMENTATION

uses System.RegularExpressions, System.SysUtils;

constructor tTimer16List.Create (_pic_info: TPICInfo);
   var
      sfr: TSFR;
      regex: TRegEx;
      m: TMatch;
      i, tmr_no: integer;
      done: boolean;
   begin
      regex := TRegEx.Create ('^TMR([0-9]*)$');

      for sfr in _pic_info.sfrs do
         if sfr.kind = joined_sfr then
            begin
               m := regex.Match (sfr.name);
               if m.Success
               then
                  begin
                     tmr_no := 0;
                     for i := 1 to Length(m.Groups[1].Value) do
                        tmr_no := (tmr_no*10) + ord(m.Groups[1].Value[i]) - ord('0');
                     i := Length(timers);
                     SetLength (timers, i+1);
                     timers[i] := tmr_no
                  end
         end;

      // sort timers
      repeat
         done := true;
         for i := 0 to Length(timers)-2 do
            if timers[i] > timers[i+1] then
               begin
                  tmr_no := timers[i];
                  timers[i] := timers[i+1];
                  timers[i+1] := tmr_no;
                  done := false
               end
      until done
   end;

procedure tTimer16List.AppendIncludeFileSource (out: TOutStringProc);
   var
      i: integer;
   begin
      for i := 0 to Length(timers)-1 do
         out (format ('procedure reset_TMR%d_cycle (cycle_count: uint16);', [timers[i]]));
      if Length(timers) > 0 then
         out ('')
   end;

procedure tTimer16List.AppendXMLFile (out: TOutStringProc);
   var
      i: integer;
   begin
      if Length(timers) > 0 then
         begin
            out ('   <Timers16Bit>');
            for i := 0 to Length(timers)-1 do
               out (format ('      <Timer number="%d"/>', [timers[i]]));
            out ('   </Timers16Bit>')
         end
   end;

END.

