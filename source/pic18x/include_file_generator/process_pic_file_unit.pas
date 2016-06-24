UNIT process_pic_file_unit;

INTERFACE

uses
   common_unit, all_pic18x_sfr_field_info_unit, combo_type_unit;

procedure process_pic_file (pic_file_name: string;
                            var is_extended: boolean;
                            output_source_line, output_xml_line, output_error: TOutStringProc;
                            var pic_sfr_field_info: tMicroControllerSFRFieldInfo;
                            combo_type_list: TComboTypeList
                           );

IMPLEMENTATION

uses
   pic18x_information_unit, pic_file_parser_unit, pic18x_interrupt_variable_unit,
   pic18x_config_bits_unit, pic18x_16bit_timer_unit, win32_utils,
   pic18x_typedef_unit, pic18x_ioreg_unit, main_form_unit, System.SysUtils,
   pic18x_dataspace_unit;

procedure process_pic_file (pic_file_name: string;
                            var is_extended: boolean;
                            output_source_line, output_xml_line, output_error: TOutStringProc;
                            var pic_sfr_field_info: tMicroControllerSFRFieldInfo;
                            combo_type_list: TComboTypeList
                           );
   var
      pic_info: TPICInfo;
      config_bits: tConfigBits;
      ioreg_list: tIoregList;
      typedefs: tTypeDefList;
      interrupt_variables: t_interrupt_variables;
      timers: tTimer16List;
      dataspacelist: tDataSpaceList;
      pic_file_date : TDateTime;
   begin
      try
         pic_info := nil;
         config_bits := nil;
         ioreg_list := nil;
         typedefs := nil;
         interrupt_variables := nil;
         timers := nil;
         dataspacelist := nil;
         try
            pic_info := parse_pic_file (pic_file_directory + pic_file_name);
            if  pic_info.is_extended then
               begin
                  pic_sfr_field_info := tMicroControllerSFRFieldInfo.CreateFromPICInfo (pic_info);

                  is_extended := true;
                  config_bits := tConfigBits.Create (pic_info);
                  typedefs := tTypeDefList.Create;
                  ioreg_list := tIoregList.Create (pic_info, pic_sfr_field_info, typedefs, combo_type_list);
                  interrupt_variables := t_interrupt_variables.Create (pic_info);
                  timers := tTimer16List.Create (pic_info);
                  dataspacelist := tDataSpaceList.Create (pic_info);

                  config_bits.AppendIncludeFileSource (output_source_line);
                  typedefs.AppendIncludeFileSource (output_source_line);
                  ioreg_list.AppendIncludeFileSource (output_source_line);
                  interrupt_variables.AppendIncludeFileSource (output_source_line);
                  timers.AppendIncludeFileSource (output_source_line);

                  output_xml_line ('<?xml version="1.0" encoding="UTF-8"?>');
                  output_xml_line (format ('<Microcontroller name="%s">', [pic_info.chip_name]));
                  output_xml_line (format ('   <Generator program="%s" version="%s"/>',
                                           [ExtractFileName(ParamStr(0)),
                                            GetVersionInfo('FileVersion')
                                           ]
                                          )
                                  );
                  FileAge (pic_file_directory + pic_file_name, pic_file_date);
                  output_xml_line (format ('   <Source filename="%s" mplabx_version="%s" filedate="%s"/>',
                                           [pic_file_name,
                                            '3.23',
                                            FormatDateTime ('YYYY-MM-DD', pic_file_date)
                                           ]
                                          )
                                  );
                  pic_info.AppendXMLFile(output_xml_line);
                  config_bits.AppendXMLFile(output_xml_line);
                  typedefs.AppendXMLFile(output_xml_line);
                  timers.AppendXMLFile(output_xml_line);
                  dataspacelist.AppendXMLFile(output_xml_line);

                  output_xml_line ('</Microcontroller>')
               end
            else
               is_extended := false
         finally
            pic_info.Free;
            config_bits.Free;
            typedefs.Free;
            ioreg_list.Free;
            interrupt_variables.Free;
            timers.Free;
            dataspacelist.Free
         end
      except
         on e: EAssertionFailed do
            output_error (pic_file_name + ' ' + e.Message)
      end
   end;

END.
