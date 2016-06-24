UNIT pic18x_dataspace_unit;

INTERFACE

uses
   pic18x_information_unit, common_unit;

type
   tDataSpaceList =
      class
         constructor Create (_pic_info: TPICInfo);
         procedure AppendXMLFile (out: TOutStringProc);
      private
         pic_info: TPICInfo
      end;

IMPLEMENTATION

uses System.RegularExpressions, System.SysUtils;

constructor tDataSpaceList.Create (_pic_info: TPICInfo);
   var
      ds: TDataSpaceSector;
      done: boolean;
      i, j: integer;
   begin
      pic_info := _pic_info;

      with pic_info do
         begin
            // sort in ascending order
            repeat
               done := true;
               for i := 0 to Length(data_space_sectors)-2 do
                  if data_space_sectors[i].beginaddr > data_space_sectors[i+1].beginaddr then
                     begin
                        ds := data_space_sectors[i];
                        data_space_sectors[i] := data_space_sectors[i+1];
                        data_space_sectors[i+1] := ds;
                        done := false
                     end
            until done;

            // merge adjacent
            i := 0;
            while i < Length(data_space_sectors)-1 do
               if (data_space_sectors[i].kind = data_space_sectors[i+1].kind)
                  and
                  (data_space_sectors[i].endaddr = data_space_sectors[i+1].beginaddr)
               then
                  begin
                     data_space_sectors[i].endaddr := data_space_sectors[i+1].endaddr;
                     data_space_sectors[i+1].Free;
                     for j := i+1 to Length(data_space_sectors)-2 do
                        data_space_sectors[j] := data_space_sectors[j+1];
                     SetLength (data_space_sectors, Length(data_space_sectors)-1)
                  end
               else
                  i := i + 1
         end
   end;

procedure tDataSpaceList.AppendXMLFile (out: TOutStringProc);
   function ds_kind (k: TDataSpaceSectorKind): string;
      begin
         case k of
            gpr_dataspace_sector:
               result := 'GPR_DataSpace';
            sfr_dataspace_sector:
               result := 'SFR_DataSpace';
            dpr_dataspace_sector:
               result := 'DPR_DataSpace';
         else
            assert (false)
         end
      end;
   var
      i: integer;
   begin
      out ('   <DataSpaces>');
      with pic_info do
         for i := 0 to Length(data_space_sectors)-1 do
            out (format('      <%s beginaddr="%d" endaddr="%d"/>',
                        [ds_kind(data_space_sectors[i].kind), data_space_sectors[i].beginaddr, data_space_sectors[i].endaddr]
                       )
                );
      out ('   </DataSpaces>')
   end;

END.

