UNIT all_pic18x_sfr_field_info_unit;

INTERFACE

uses
  System.SysUtils, System.Classes,
  pic18x_information_unit, Generics.Collections, Generics.Defaults,
  LibXmlParser, common_unit;

type
   tFileVersion = uint16;
   tFieldMode = string[10];       // max seen 5
   tFieldName = string[25];       // max seen 19
   tFieldInfo =
      record
         mode: tFieldMode;
         field_name: tFieldName;
         bitno: uint8;
         width: uint8
      end;
   tSFRName = string[20];         // max seen 11
   tSFRInfo =
      record
         sfr_name: tSFRName;
         fields: array of tFieldInfo;
         sfr: TSFRDef
      end;
   tSFRAddressSlot =
      class
         addr: uint16;
         sfrs: array of tSFRInfo
      end;
   tMicrocontrollerName = string[25];  // max seen 14
   tMicroControllerSFRFieldInfo =
      class
         microcontroller_name: tMicrocontrollerName;
         sfr_addr_slots: TObjectList<tSFRAddressSlot>;
         constructor Create;
         constructor CreateFromPICInfo (pic_info: TPICInfo);
         destructor Destroy;
            override;
      end;
   eInvalidBinaryDateFile = class (Exception);
   tAllPIC18xInfo =
      class(TObjectList<tMicroControllerSFRFieldInfo>)
         constructor CreateEmpty;
         constructor CreateFromBinaryDataFile;
         procedure OutputBinaryDataFile;
      end;

var
   max_picname_len: uint8;
   max_sfrname_len: uint8;
   max_modename_len: uint8;
   max_fieldname_len: uint8;


IMPLEMENTATION

uses
   System.Variants, LibXmlComps, System.Math, pic18x_selection_dialog_unit;

{%CLASSGROUP 'Vcl.Controls.TControl'}

const
   current_bin_file_version = 1;  // increment whenever changes invalidate old files
   header_string = 'all pic18x sfr field info';
var
   all_pic_info_file_name: string;
   header: string[length(header_string)];

//===============================
//  tMicroControllerSFRFieldInfo

constructor tMicroControllerSFRFieldInfo.Create;
   begin
      sfr_addr_slots := TObjectList<tSFRAddressSlot>.Create;
      inherited
   end;

constructor tMicroControllerSFRFieldInfo.CreateFromPICInfo (pic_info: TPICInfo);
   var
      current_sfr_idx: integer;
      extended_addr_slots: TObjectList<tSFRAddressSlot>;

   procedure record_sfr (sfr: TSFRDef);
      var
         mode: TSFRMode;
         field: TSFRField;
         addr, bitno: integer;
         addr_slot: tSFRAddressSlot;
         field_idx: integer;
      begin
         if (sfr <> nil)
            and
            (not is_cpu_reg (sfr.name))
         then
            begin
               if sfr.IsAlternateSharedAddressSFR then
                  begin
                     addr := sfr.addr + $1000;
                     if (extended_addr_slots.Count = 0)
                        or
                        (extended_addr_slots[extended_addr_slots.Count-1].addr < addr)
                     then
                        begin
                           addr_slot := tSFRAddressSlot.Create;
                           extended_addr_slots.Add (addr_slot);
                           addr_slot.addr := addr
                        end
                     else
                        addr_slot := extended_addr_slots[extended_addr_slots.Count-1]
                  end
               else
                  begin
                     addr := sfr.addr;
                     if (sfr_addr_slots.Count = 0)
                        or
                        (sfr_addr_slots[sfr_addr_slots.Count-1].addr < addr)
                     then
                        begin
                           addr_slot := tSFRAddressSlot.Create;
                           sfr_addr_slots.Add (addr_slot);
                           sfr_addr_slots[sfr_addr_slots.Count-1].addr := addr
                        end
                     else
                        addr_slot := sfr_addr_slots[sfr_addr_slots.Count-1]
                  end;

               current_sfr_idx := Length(addr_slot.sfrs);
               SetLength(addr_slot.sfrs, current_sfr_idx+1);
               addr_slot.sfrs[current_sfr_idx].sfr_name := ShortString(sfr.name);
               addr_slot.sfrs[current_sfr_idx].sfr := sfr;
               max_sfrname_len := max (max_sfrname_len, Length(sfr.name));

               for mode in sfr.modes do
                  begin
                     bitno := -1;
                     for field in mode.fields do
                        begin
                           bitno := bitno + field.width;

                           if field.fieldname <> '' then
                              begin
                                 field_idx := Length(addr_slot.sfrs[current_sfr_idx].fields);
                                 SetLength (addr_slot.sfrs[current_sfr_idx].fields, field_idx+1);

                                 addr_slot.sfrs[current_sfr_idx].fields[field_idx].mode := ShortString(mode.id);
                                 addr_slot.sfrs[current_sfr_idx].fields[field_idx].field_name := ShortString(field.fieldname);
                                 addr_slot.sfrs[current_sfr_idx].fields[field_idx].bitno := bitno;
                                 addr_slot.sfrs[current_sfr_idx].fields[field_idx].width := field.width;

                                 max_modename_len := max (max_modename_len, Length(mode.id));
                                 max_fieldname_len := max (max_fieldname_len, Length(field.fieldname))
                              end
                        end
                  end
            end
      end;

   var
      sfr: TSFR;
      jsfr: TJoinedSFR;
      i: integer;
   begin
      microcontroller_name := ShortString(pic_info.chip_name);
      max_picname_len := max (max_picname_len, Length(pic_info.chip_name));
      sfr_addr_slots := TObjectList<tSFRAddressSlot>.Create (true);
      extended_addr_slots := TObjectList<tSFRAddressSlot>.Create (false);

      for sfr in pic_info.sfrs do
         case sfr.kind of
            simple_sfr:
               record_sfr (TSFRDef(sfr));
            joined_sfr:
               begin
                  jsfr := TJoinedSFR(sfr);
                  case jsfr.sfrL.kind of
                     simple_sfr:
                        begin
                           record_sfr (TSFRDef (jsfr.sfrL));
                           record_sfr (TSFRDef (jsfr.sfrH));
                           record_sfr (TSFRDef (jsfr.sfrU))
                        end;
                     muxd_sfr:
                        for i := 0 to jsfr.var_count-1 do
                           begin
                              assert (jsfr.sfrU = nil);
                              record_sfr (TMuxdSFR(jsfr.sfrL).sfrs[i]);
                              record_sfr (TMuxdSFR(jsfr.sfrH).sfrs[i])
                           end;
                  else
                     assert (false)
                  end
               end;
            muxd_sfr:
               for i := 0 to sfr.var_count-1 do
                  record_sfr (TMuxdSFR(sfr).sfrs[i]);
         else
            assert (false)
         end;

      for i := 0 to extended_addr_slots.Count-1 do
         sfr_addr_slots.Add (extended_addr_slots[i]);

      extended_addr_slots.Free
   end;

destructor tMicroControllerSFRFieldInfo.Destroy;
   begin
      sfr_addr_slots.Free;
      inherited
   end;


//=================
//  tAllPIC18xInfo

constructor tAllPIC18xInfo.CreateEmpty;
   begin
      inherited Create;
      max_picname_len := 0;
      max_sfrname_len := 0;
      max_modename_len := 0;
      max_fieldname_len := 0
   end;

constructor tAllPIC18xInfo.CreateFromBinaryDataFile;
   var
      f: TMemoryStream;
      file_version: tFileVersion;
      current_microcontroller: tMicroControllerSFRFieldInfo;
      current_addr_slot: tSFRAddressSlot;
      current_sfr_idx: integer;
      sfr_name: tSFRName;
      current_field_idx: integer;
      mode: tFieldMode;
      file_header: string[length(header_string)];
   begin
      f := tMemoryStream.Create;
      try
         try
            f.LoadFromFile (all_pic_info_file_name)
         except
            on e: EFOpenError do
               raise eInvalidBinaryDateFile.Create (e.Message)
         end;
         f.Read (file_header, sizeof(file_header));
         if file_header <> header_string then
            raise eInvalidBinaryDateFile.Create ('incorrect header string');
         f.Read (file_version, sizeof(file_version));
         if file_version <> current_bin_file_version then
            raise eInvalidBinaryDateFile.Create ('incorrect version');
         inherited Create;
         f.Read (max_picname_len, sizeof(max_picname_len));
         f.Read (max_sfrname_len, sizeof(max_sfrname_len));
         f.Read (max_modename_len, sizeof(max_modename_len));
         f.Read (max_fieldname_len, sizeof(max_fieldname_len));
         current_microcontroller := tMicroControllerSFRFieldInfo.Create;
         f.Read (current_microcontroller.microcontroller_name, sizeof(current_microcontroller.microcontroller_name));
         while current_microcontroller.microcontroller_name <> '' do
            begin
               Add (current_microcontroller);
               current_addr_slot := tSFRAddressSlot.Create;
               f.Read (current_addr_slot.addr, sizeof(current_addr_slot.addr));
               while current_addr_slot.addr <> 0 do
                  begin
                     current_microcontroller.sfr_addr_slots.Add(current_addr_slot);
                     current_sfr_idx := -1;
                     f.Read (sfr_name, sizeof(sfr_name));
                     while sfr_name <> '' do
                        begin
                           current_sfr_idx := current_sfr_idx + 1;
                           SetLength(current_addr_slot.sfrs, current_sfr_idx+1);
                           current_addr_slot.sfrs[current_sfr_idx].sfr_name := sfr_name;
                           current_field_idx := -1;
                           f.Read (mode, sizeof(mode));
                           while mode <> '' do
                              begin
                                 current_field_idx := current_field_idx + 1;
                                 with current_addr_slot.sfrs[current_sfr_idx] do
                                    begin
                                       SetLength (fields, current_field_idx+1);
                                       fields[current_field_idx].mode := mode;
                                       with fields[current_field_idx] do
                                          begin
                                             f.Read (field_name, sizeof(field_name));
                                             f.Read (bitno, sizeof(bitno));
                                             f.Read (width, sizeof(width))
                                          end
                                    end;
                                 f.Read (mode, sizeof(mode))
                              end;
                           f.Read (sfr_name, sizeof(sfr_name))
                        end;
                     current_addr_slot := tSFRAddressSlot.Create;
                     f.Read (current_addr_slot.addr, sizeof(current_addr_slot.addr))
                  end;
               current_addr_slot.Free;
               current_microcontroller := tMicroControllerSFRFieldInfo.Create;
               f.Read (current_microcontroller.microcontroller_name, sizeof(current_microcontroller.microcontroller_name))
            end;
         current_microcontroller.Free
      finally
         f.Free
      end
   end;

procedure tAllPIC18xInfo.OutputBinaryDataFile;
   var
      f: TMemoryStream;
      microcontroller: tMicroControllerSFRFieldInfo;
      addr_slot: tSFRAddressSlot;
      i, j: integer;
      blank: string[255];
      zero: uint16;
      file_version: tFileVersion;
   begin
      file_version := current_bin_file_version;
      zero := 0;  assert (sizeof(zero) = sizeof(addr_slot.addr));
      blank := '';

      Sort (TComparer<tMicroControllerSFRFieldInfo>.Construct
               (function (const L, R: tMicroControllerSFRFieldInfo): integer
                   begin
                      result := ComparePICNames (String(L.microcontroller_name), String(R.microcontroller_name))
                   end
               )
           );

      f := TMemoryStream.Create;
      f.Write (header, sizeof(header));
      f.Write (file_version, sizeof(file_version));
      f.Write (max_picname_len, sizeof(max_picname_len));
      f.Write (max_sfrname_len, sizeof(max_sfrname_len));
      f.Write (max_modename_len, sizeof(max_modename_len));
      f.Write (max_fieldname_len, sizeof(max_fieldname_len));
      for microcontroller in Self do
         begin
            f.Write (microcontroller.microcontroller_name, sizeof(microcontroller.microcontroller_name));
            for addr_slot in microcontroller.sfr_addr_slots do
               begin
                  f.Write (addr_slot.addr, sizeof(addr_slot.addr));
                  for i := 0 to Length(addr_slot.sfrs)-1 do
                     begin
                        f.Write (addr_slot.sfrs[i].sfr_name, sizeof(addr_slot.sfrs[i].sfr_name));
                        for j := 0 to Length(addr_slot.sfrs[i].fields)-1 do
                           begin
                              f.Write (addr_slot.sfrs[i].fields[j].mode, sizeof(addr_slot.sfrs[i].fields[j].mode));
                              f.Write (addr_slot.sfrs[i].fields[j].field_name, sizeof(addr_slot.sfrs[i].fields[j].field_name));
                              f.Write (addr_slot.sfrs[i].fields[j].bitno, sizeof(addr_slot.sfrs[i].fields[j].bitno));
                              f.Write (addr_slot.sfrs[i].fields[j].width, sizeof(addr_slot.sfrs[i].fields[j].width));
                           end;
                        f.Write (blank, sizeof(addr_slot.sfrs[i].fields[j].mode));
                     end;
                  f.Write (blank, sizeof(addr_slot.sfrs[i].sfr_name));
               end;
            f.Write (zero, sizeof(addr_slot.addr));
         end;
      f.write (blank, sizeof(microcontroller.microcontroller_name));
      f.SaveToFile (all_pic_info_file_name);
      f.Free
   end;


INITIALIZATION
   all_pic_info_file_name := ExtractFilePath(ParamStr(0)) + 'pic18x\all_pic_sfr_field_info.bin';
   header := header_string;

END.
