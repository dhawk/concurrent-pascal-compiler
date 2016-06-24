UNIT pic18x_config_bits_unit;

INTERFACE

uses
   pic18x_information_unit, common_unit;

type
   tConfigBits =
      class
         constructor Create (_pic_info: TPICInfo);
         procedure AppendIncludeFileSource (out: TOutStringProc);
         procedure AppendXMLFile (out: TOutStringProc);
      private
         pic_info: TPICInfo;
      end;

IMPLEMENTATION

uses
  System.SysUtils, System.Classes, pic18x_common_unit;

constructor tConfigBits.Create (_pic_info: TPICInfo);
   begin
      pic_info := _pic_info
   end;

procedure tConfigBits.AppendIncludeFileSource (out: TOutStringProc);

   procedure out_field_enum (b: TConfigByte; f: TConfigByteField; term_char: char);

      function fmt (value, width: integer): string;
         begin
            result := '';
            if width > 4 then
               result := format ('$%2.2X', [value])
            else if width > 3 then
               result := format ('$%1.1X', [value])
            else
               result := IntToStr (value)
         end;

      const
         indent = '               ';
      var
         s: string;
         TFuseByteFieldValue, j, id_width, last: integer;
         changed: boolean;
         temp: TFuseByteFieldValues;
      begin   // out_field_enum
         out (indent + f.name + ':');
         s := indent + '   (';
         id_width := 0;

         repeat
            changed := false;
            for TFuseByteFieldValue := 0 to Length(f.values)-2 do
               if f.values[TFuseByteFieldValue].value < f.values[TFuseByteFieldValue+1].value then
                  begin
                     temp := f.values[TFuseByteFieldValue];
                     f.values[TFuseByteFieldValue] := f.values[TFuseByteFieldValue+1];
                     f.values[TFuseByteFieldValue+1] := temp;
                     changed := true
                  end
         until not changed;

         for TFuseByteFieldValue := Length(f.values)-1 downto 0 do
            if not f.values[TFuseByteFieldValue].islanghidden then
               if Length(config_bits_field_enum_name(b.name, f.name, f.values[TFuseByteFieldValue].cname)) > id_width then
                  id_width := Length(config_bits_field_enum_name(b.name, f.name, f.values[TFuseByteFieldValue].cname));
         last := 0;
         for TFuseByteFieldValue := Length(f.values)-1 downto 0 do
            if not f.values[TFuseByteFieldValue].islanghidden then
               last := TFuseByteFieldValue;
         for TFuseByteFieldValue := Length(f.values)-1 downto 0 do
            if not f.values[TFuseByteFieldValue].islanghidden then
               begin
                  s := s + config_bits_field_enum_name(b.name, f.name, f.values[TFuseByteFieldValue].cname);
                  for j := Length(config_bits_field_enum_name(b.name, f.name, f.values[TFuseByteFieldValue].cname)) to id_width do
                     s := s + ' ';
                  s := s + '= ' + fmt (f.values[TFuseByteFieldValue].value, f.width);
                  if TFuseByteFieldValue > last then
                     s := s + ',';
                  out (s);
                  s := indent + '    '
               end;
         out (indent + '   )' + term_char)
      end;  // out_field_enum

   var
      i,j,w: integer;
      s: string;
   begin   // output_config_fuses_type
      out ('type');
      out ('   ' + config_bits_constant_type_name(pic_info.chip_name) + ' =');
      out ('      record');
      for i := 0 to Length(pic_info.config_bytes)-1 do
         if Length(pic_info.config_bytes[i].fields) = 0 then
            if i < length(pic_info.config_bytes)-1 then
               out ('         -: uint8;')
            else
               out ('         -: uint8')
         else
            begin
               out ('         ' + pic_info.config_bytes[i].name + ':');
               out ('            packed record');
               j := Length(pic_info.config_bytes[i].fields)-1;
               while j >= 0 do
                  if pic_info.config_bytes[i].fields[j].nop_marker then
                     begin
                        s := '               nop: (' + pic_info.config_bytes[i].name +  '_nop = $F)';
                        if j > 0 then
                           out (s + ';')
                        else
                           out (s);
                        j := j - 1
                     end
                  else if pic_info.config_bytes[i].fields[j].empty_field
                       then
                          begin
                             w := 0;
                             repeat
                                w := w + pic_info.config_bytes[i].fields[j].width;
                                j := j - 1
                             until (j < 0)
                                   or
                                   (not pic_info.config_bytes[i].fields[j].empty_field);
                             if j >= 0 then
                                out ('               -: uint' + IntToStr(w) + ';')
                             else
                                out ('               -: uint' + IntToStr(w))
                          end
                  else
                     begin
                        if j > 0 then
                           out_field_enum (pic_info.config_bytes[i], pic_info.config_bytes[i].fields[j], ';')
                        else
                           out_field_enum (pic_info.config_bytes[i], pic_info.config_bytes[i].fields[j], ' ');
                        j := j - 1
                     end;
               if i < length(pic_info.config_bytes)-1 then
                  out ('            end;')
               else
                  out ('            end')
            end;
      out ('      end;');
      out ('')
   end;   // output_config_fuses_type


procedure tConfigBits.AppendXMLFile (out: TOutStringProc);
   function boolean_value (b: boolean): string;
      begin
         if b then
            result := 'true'
         else
            result := 'false'
      end;
   var
      b: TConfigByte;
      f: TConfigByteField;
      v: TFuseByteFieldValues;
      s: string;
   begin
      out ('   <ConfigBits>');
      for b in pic_info.config_bytes do
         if Length(b.fields) = 0 then
            begin
               assert (b.name = '');
               out ('      <ConfigByte/>')
            end
         else
            begin
               out ('      <ConfigByte name="' + b.name + '">');
               for f in b.fields do
                  begin
                     s := format ('         <ConfigField name="%s" desc="%s" default="%d" ishidden="%s" nop_marker="%s"',
                                  [f.name,
                                   sanitize_xml_strings(f.desc),
                                   f.default_value,
                                   boolean_value(f.ishidden),
                                   boolean_value(f.nop_marker)
                                  ]
                                 );
                     if Length(f.values) = 0 then
                        out (s + '/>')
                     else
                        begin
                           out (s + '>');
                           for v in f.values do
                              if v.cname <> '' then
                                 out (format ('            <ConfigFieldValue name="%s" desc="%s" value="%d"/>',
                                              [v.cname,
                                               sanitize_xml_strings(v.desc),
                                               v.value
                                              ]
                                             )
                                     );
                           out ('         </ConfigField>')
                        end
                  end;
               out ('      </ConfigByte>')
            end;
      out ('   </ConfigBits>')
   end;

END.

