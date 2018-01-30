UNIT pic18x_microprocessor_information_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
   cpc_source_analysis_unit,
   LibXmlParser,
   wirth_balanced_binary_tree_unit;

const
   max_ram_addr = $FFF;   // 4K

const
   BSR      = $FE0;
   FSR0H    = $FEA;
   FSR0L    = $FE9;
   FSR1H    = $FE2;
   FSR1L    = $FE1;
   FSR2H    = $FDA;
   FSR2L    = $FD9;
   INDF0    = $FEF;
   INDF1    = $FE7;
   INDF2    = $FDF;
   INTCON   = $FF2;
   PCL      = $FF9;
   PCLATH   = $FFA;
   PCLATU   = $FFB;
   PLUSW0   = $FEB;
   PLUSW1   = $FE3;
   PLUSW2   = $FDB;
   POSTDEC0 = $FED;
   POSTDEC1 = $FE5;
   POSTDEC2 = $FDD;
   POSTINC0 = $FEE;
   POSTINC1 = $FE6;
   POSTINC2 = $FDE;
   PREINC0  = $FEC;
   PREINC1  = $FE4;
   PREINC2  = $FDC;
   PRODH    = $FF4;
   PRODL    = $FF3;
   STATUS   = $FD8;
   STKPTR   = $FFC;
   TABLAT   = $FF5;
   TBLPTRH  = $FF7;
   TBLPTRL  = $FF6;
   TBLPTRU  = $FF8;
   TOSH     = $FFE;
   TOSL     = $FFD;
   TOSU     = $FFF;
   WREG     = $FE8;

type
   TDataMemoryAddress = 0..$1fff;    // $1--- bit is for alternate address SFRs (rare)
   tDateRegionType = (gpr_region, sfr_region, dpr_region);
   t_pic_info =
      class
         microprocessor: string;
         available_program_memory: integer;
         available_SRAM: integer;
         available_eeprom_memory: integer;
         first_access_bank_absolute_address: integer;
         RCON: integer;
         EEADR: integer;
         EEADRH: integer;
         EECON1: integer;
         EECON2: integer;
         EEDATA: integer;
         {$ifdef INCLUDE_SIMULATION}
         // for tests using PIC18F2520 & PIC18F65J94
         TMR3H: integer;
         TMR3L: integer;
         T3CON: integer;
         // for tests using PIC18F65J94
         UFRMH: integer;
         UFRML: integer;
         // for tests using PIC18F65J50
         WDTCON: integer;
         OSCCON: integer;
         REFOCON: integer;   // an alternate shared address SFR
         {$endif}
         data_regions:
            array of
               record
                  region_type: tDateRegionType;
                  start_addr, end_addr: integer
               end;
         ipen_bit_location: (unknown_ipen_bit_location, rcon_bit7, intcon_bit5);
         sixteen_bit_timers: array of integer;
         constructor Create (xml_fn: string);
         constructor CreateWithDefaults;
         procedure validate_pic18_ioreg_address (name: string; addr, size: integer; src_loc: TSourceLocation);
         function SFR_Name (sfr_addr: integer): string;
         function SFR_Address (_sfr_name: string): integer;
         function IsReversedType (typ: string): boolean;
         destructor Destroy;
            override;
      private
         sfr_names: TBalancedBinaryTree;
         fixed_sfrs: set of 0..255;
         reversed_types: array of string;
         procedure add_sfr_name (addr: integer; name: string);
         procedure add_fixed_SFR_Names;
         procedure add_data_region (typ: tDateRegionType; start_addr, end_addr: integer);
         procedure XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
      end;

const
   status_n_bit_mask  = $10;
   status_ov_bit_mask = $08;
   status_z_bit_mask  = $04;
   status_dc_bit_mask = $02;
   status_c_bit_mask  = $01;

   status_n  = 4;
   status_ov = 3;
   status_z  = 2;
   status_dc = 1;
   status_c  = 0;

   status_equals       = $04;   // N=0, Z=1
   status_less_than    = $10;   // N=1, Z=0
   status_greater_than = $00;   // N=0, Z=0

   eecon1_eepgd = 7;
   eecon1_cfgs  = 6;
   eecon1_wren  = 2;
   eecon1_wr    = 1;
   eecon1_rd    = 0;

   intcon_gieh =  7;
   intcon_giel =  6;

   rcon_ipen = 7;

const
   upspecified_microprocessor = '*unspecified*';
   err_fixed_sfr_conflict = 'address conflict with CPU register %s';
   err_not_in_SFR_region = 'address is not in PIC18 SFR region';
   err_illegal_address = 'illegal SFR address';

procedure create_pic_info (proc: string);
function pic_info: t_pic_info;
procedure pic_info_Free;

IMPLEMENTATION

uses
   cpc_common_unit,
   LibXmlComps,
   SysUtils;

var f_pic_info: t_pic_info;

function pic_info: t_pic_info;
   begin
      if f_pic_info = nil then
         f_pic_info := t_pic_info.CreateWithDefaults;
      result := f_pic_info
   end;

procedure pic_info_Free;
   begin
      f_pic_info.Free;
      f_pic_info := nil
   end;

function t_pic_info.IsReversedType (typ: string): boolean;
   var i: integer;
   begin
      result := true;
      for i := 0 to Length(reversed_types)-1 do
         if typ = reversed_types[i] then
            exit;
      result := false
   end;

procedure t_pic_info.add_data_region (typ: tDateRegionType; start_addr, end_addr: integer);
   var
      i: integer;
   begin
      i := Length(data_regions);
      SetLength(data_regions, i+1);
      data_regions[i].region_type := typ;
      data_regions[i].start_addr := start_addr;
      data_regions[i].end_addr := end_addr
   end;

procedure t_pic_info.validate_pic18_ioreg_address (name: string; addr, size: integer; src_loc: TSourceLocation);
   function fixed_sfr (addr: integer): boolean;
      begin
         result := ((addr and $F00) = $F00)
                   and
                   ((addr and $0FF) in fixed_sfrs)
      end;
   var
      i: integer;
   begin
      // some processors use ADSHR to multiplex SFR addresses, this is represented by $1000 added to addr
      if (addr and $FFF) > $1000 then
         raise compile_error.Create (err_illegal_address, src_loc);

      addr := addr and $FFF;

      for i := 0 to size-1 do
         if fixed_sfr ((addr+i) and $FFF)
            and
            not (((i=0) and(UpperCase(name) = pic_info.SFR_Name (addr)))
                 or
                 ((i=0) and (UpperCase(name)+'L' = pic_info.SFR_Name (addr)))
                 or
                 ((i=1) and (UpperCase(name)+'H' = pic_info.SFR_Name (addr+1)))
                 or
                 ((i=2) and (UpperCase(name)+'U' = pic_info.SFR_Name (addr+2)))
                )
         then
            raise compile_error.Create (format (err_fixed_sfr_conflict, [pic_info.SFR_Name (addr+i)]), src_loc);

      for i := 0 to Length(data_regions)-1 do
         if data_regions[i].region_type = sfr_region then
            begin
               if (data_regions[i].start_addr <= addr)
                  and
                  (addr + size - 1 < data_regions[i].end_addr)
               then
                  exit   // addr is within an SFR region
            end;

      raise compile_error.Create (err_not_in_SFR_region, src_loc)
   end;

type
   tSFREntry =
      class (TBalancedTreeEntry)
         addr: integer;
         name: string;
         function compare
            (a: TBalancedTreeEntry
            ): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy (ToA: TBalancedTreeEntry);
            override;
         constructor Create (_addr: integer; _name: string);
      end;

constructor tSFREntry.Create (_addr: integer; _name: string);
   begin
      inherited Create;
      addr := _addr;
      name := _name
   end;

function tSFREntry.compare
   (a: TBalancedTreeEntry
   ): Shortint;  // a < self :-1  a=self :0  a > self :+1
   begin   // tree is sorted by address
      if tSFREntry(a).addr < addr then
         result := -1
      else if tSFREntry(a).addr = addr then
         result := 0
      else
         result := 1
   end;

procedure tSFREntry.copy (ToA: TBalancedTreeEntry);
   begin
      tSFREntry(ToA).addr := addr;
      tSFREntry(ToA).name := name
   end;

function t_pic_info.SFR_Address (_sfr_name: string): integer;    // slow!
   procedure search (e: tSFREntry);
      begin
         if _sfr_name = e.name then
            result := e.addr
         else
            begin
               if e.lesser_values <> nil then
                  search (tSFREntry(e.lesser_values));
               if e.greater_values <> nil then
                  search (tSFREntry(e.greater_values))
            end
      end;
   begin
      result := -1;
      search (tSFREntry(sfr_names.root))
   end;

procedure create_pic_info (proc: string);
   const
      include_file_directory = 'pic18x' + PathDelim + 'processor_definition_files' + PathDelim;
   begin
      f_pic_info.Free;
      f_pic_info := t_pic_info.Create (ExtractFilePath(ParamStr(0)) + include_file_directory + proc + '.xml');
   end;

function t_pic_info.SFR_Name (sfr_addr: integer): string;
   function find
      (item: TBalancedTreeEntry
      ): TBalancedTreeEntry;
      begin
         if item = nil then
            result := nil
         else if tSFREntry(item).addr = sfr_addr then
            result := item
         else if tSFREntry(item).addr < sfr_addr then
            result := find(item.greater_values)
         else
            result := find(item.lesser_values)
      end;
   var
      p: TBalancedTreeEntry;
   begin
      if sfr_names = nil then
         result := format('0%3.3Xh', [sfr_addr])
      else
         begin
            p := find(sfr_names.root);
            if p = nil then
               result := format('0%3.3Xh', [sfr_addr])
            else
               result := tSFREntry(p).name
         end;
   end;

procedure t_pic_info.add_sfr_name (addr: integer; name: string);
   begin
      if addr >= 0 then
         sfr_names.add (tSFREntry.Create (addr, name))
   end;

procedure t_pic_info.add_fixed_SFR_Names;
   var
      total: integer;
   procedure add_fixed_sfr (addr: integer);
      begin
         if addr > 0 then
            begin
               fixed_sfrs := fixed_sfrs + [addr and $FF];
               total := total + 1
            end
      end;
   const
      number_of_fixed_sfrs = 38;
   begin
      total := 0;
      add_sfr_name (BSR, 'BSR'); total := total + 1;
      add_sfr_name (FSR0H, 'FSR0H'); total := total + 1;
      add_sfr_name (FSR0L, 'FSR0L'); total := total + 1;
      add_sfr_name (FSR1H, 'FSR1H'); total := total + 1;
      add_sfr_name (FSR1L, 'FSR1L'); total := total + 1;
      add_sfr_name (FSR2H, 'FSR2H'); total := total + 1;
      add_sfr_name (FSR2L, 'FSR2L'); total := total + 1;
      add_sfr_name (INDF0, 'INDF0'); total := total + 1;
      add_sfr_name (INDF1, 'INDF1'); total := total + 1;
      add_sfr_name (INDF2, 'INDF2'); total := total + 1;
      add_sfr_name (INTCON, 'INTCON'); total := total + 1;
      add_sfr_name (PCL, 'PCL'); total := total + 1;
      add_sfr_name (PCLATH, 'PCLATH'); total := total + 1;
      add_sfr_name (PCLATU, 'PCLATU'); total := total + 1;
      add_sfr_name (PLUSW0, 'PLUSW0'); total := total + 1;
      add_sfr_name (PLUSW1, 'PLUSW1'); total := total + 1;
      add_sfr_name (PLUSW2, 'PLUSW2'); total := total + 1;
      add_sfr_name (POSTDEC0, 'POSTDEC0'); total := total + 1;
      add_sfr_name (POSTDEC1, 'POSTDEC1'); total := total + 1;
      add_sfr_name (POSTDEC2, 'POSTDEC2'); total := total + 1;
      add_sfr_name (POSTINC0, 'POSTINC0'); total := total + 1;
      add_sfr_name (POSTINC1, 'POSTINC1'); total := total + 1;
      add_sfr_name (POSTINC2, 'POSTINC2'); total := total + 1;
      add_sfr_name (PREINC0, 'PREINC0'); total := total + 1;
      add_sfr_name (PREINC1, 'PREINC1'); total := total + 1;
      add_sfr_name (PREINC2, 'PREINC2'); total := total + 1;
      add_sfr_name (PRODH, 'PRODH'); total := total + 1;
      add_sfr_name (PRODL, 'PRODL'); total := total + 1;
      add_sfr_name (STATUS, 'STATUS'); total := total + 1;
      add_sfr_name (STKPTR, 'STKPTR'); total := total + 1;
      add_sfr_name (TABLAT, 'TABLAT'); total := total + 1;
      add_sfr_name (TBLPTRH, 'TBLPTRH'); total := total + 1;
      add_sfr_name (TBLPTRL, 'TBLPTRL'); total := total + 1;
      add_sfr_name (TBLPTRU, 'TBLPTRU'); total := total + 1;
      add_sfr_name (TOSH, 'TOSH'); total := total + 1;
      add_sfr_name (TOSL, 'TOSL'); total := total + 1;
      add_sfr_name (TOSU, 'TOSU'); total := total + 1;
      add_sfr_name (WREG, 'WREG'); total := total + 1;
      assert (total = number_of_fixed_sfrs);

      total := 0;
      fixed_sfrs := [];
      add_fixed_sfr (BSR);
      add_fixed_sfr (FSR0H);
      add_fixed_sfr (FSR0L);
      add_fixed_sfr (FSR1H);
      add_fixed_sfr (FSR1L);
      add_fixed_sfr (FSR2H);;
      add_fixed_sfr (FSR2L);
      add_fixed_sfr (INDF0);
      add_fixed_sfr (INDF1);
      add_fixed_sfr (INDF2);
      add_fixed_sfr (INTCON);
      add_fixed_sfr (PCL);;
      add_fixed_sfr (PCLATH);
      add_fixed_sfr (PCLATU);
      add_fixed_sfr (PLUSW0);
      add_fixed_sfr (PLUSW1);
      add_fixed_sfr (PLUSW2);
      add_fixed_sfr (POSTDEC0);
      add_fixed_sfr (POSTDEC1);
      add_fixed_sfr (POSTDEC2);
      add_fixed_sfr (POSTINC0);
      add_fixed_sfr (POSTINC1);
      add_fixed_sfr (POSTINC2);
      add_fixed_sfr (PREINC0);
      add_fixed_sfr (PREINC1);
      add_fixed_sfr (PREINC2);
      add_fixed_sfr (PRODH);
      add_fixed_sfr (PRODL);
      add_fixed_sfr (STATUS);
      add_fixed_sfr (STKPTR);
      add_fixed_sfr (TABLAT);
      add_fixed_sfr (TBLPTRH);
      add_fixed_sfr (TBLPTRL);
      add_fixed_sfr (TBLPTRU);
      add_fixed_sfr (TOSH);
      add_fixed_sfr (TOSL);
      add_fixed_sfr (TOSU);
      add_fixed_sfr (WREG);
      assert (total = number_of_fixed_sfrs)
   end;

constructor t_pic_info.Create (xml_fn: string);
   var
      xmlscanner:  TXmlScanner;
   begin
      sfr_names := TBalancedBinaryTree.Create;
      add_fixed_SFR_Names;
      xmlscanner := TXmlScanner.Create (nil);
      xmlscanner.OnEmptyTag := XmlScannerEmptyTag;
      XmlScanner.Filename := xml_fn;
      XmlScanner.Execute;
      xmlscanner.Free;
      if Length(data_regions) = 0 then
         available_SRAM := 0
      else  // for now the compiler doesn't use additonal GRP regions beyond the first one
         available_SRAM := data_regions[0].end_addr;
{$ifdef INCLUDE_SIMULATION}
      TMR3L := SFR_Address('TMR3L');
      TMR3H := SFR_Address('TMR3H');
      T3CON := SFR_Address('T3CON');
      UFRML := SFR_Address('UFRML');
      UFRMH := SFR_Address('UFRMH');
      WDTCON := SFR_Address('WDTCON');
      REFOCON := SFR_Address('REFOCON');
      OSCCON := SFR_Address('OSCCON');
{$endif}
   end;

constructor t_pic_info.CreateWithDefaults;
   begin
      microprocessor := '*unspecified*';
      RCON := $FD0;
      ipen_bit_location := rcon_bit7;
      sfr_names := TBalancedBinaryTree.Create;
      add_fixed_SFR_Names;
      available_program_memory := 32768;
      first_access_bank_absolute_address := $F80;
      add_data_region (gpr_region, 0, 2048);
      available_SRAM := data_regions[0].end_addr;
      add_data_region (sfr_region, $F80, $1000)
   end;

procedure t_pic_info.XmlScannerEmptyTag(Sender: TObject; TagName: String; Attributes: TAttrList);
   function int_value (attrib: string): integer;
      var
         s: string;
         i: integer;
      begin
         s := Attributes.Value(attrib);
         result := 0;
         for i := 1 to Length(s) do
            begin
               assert (s[i] in ['0'..'9']);
               result := (result*10) + ord(s[i]) - ord('0')
            end
      end;
   var
      i: integer;
   begin
      if TagName = 'Chip' then
         begin
            microprocessor := Attributes.Value('name');
            available_program_memory := int_value('rom_size');
            first_access_bank_absolute_address := int_value('first_access_bank_absolute_address');
         end
      else if TagName = 'EEPROM' then
         begin
            available_eeprom_memory := int_value('size');
            EEADR := int_value('eeadr');
            EEADRH := int_value('eeadrh');
            EECON1 := int_value('eecon1');
            EECON2 := int_value('eecon2');
            EEDATA := int_value('eedata');
         end
      else if TagName = 'IPEN_Location' then
         begin
            if (Attributes.Value('sfr') = 'RCON') and (int_value('bit') = 7) then
               ipen_bit_location := rcon_bit7
            else if (Attributes.Value('sfr') = 'INTCON') and (int_value('bit') = 5) then
               ipen_bit_location := intcon_bit5
            else
               assert (false, 'IPEN value in XML file not recognized')
         end
      else if TagName = 'SFR' then
         begin
            if Attributes.Value('name') = 'INTCON' then
               assert (INTCON = int_value('addr'))
            else if Attributes.Value('name') = 'RCON' then
               RCON := int_value('addr')
            else
               try
                  add_sfr_name (int_value('addr'), Attributes.Value('name'))
               except
                  on e: ESymbolAlreadyInSymbolTable
                  do tSFREntry(e.entry).name := ''
               end
         end
      else if TagName = 'Timer' then
         begin
            i := Length(sixteen_bit_timers);
            SetLength(sixteen_bit_timers, i+1);
            sixteen_bit_timers[i] := int_value('number')
         end
      else if TagName = 'ReversedType' then
         begin
            i := Length(reversed_types);
            SetLength(reversed_types, i+1);
            reversed_types[i] := Attributes.Value('name')
         end
      else if TagName = 'GPR_DataSpace' then
         add_data_region (gpr_region, int_value('beginaddr'), int_value('endaddr'))
      else if TagName = 'SFR_DataSpace' then
         add_data_region (sfr_region, int_value('beginaddr'), int_value('endaddr'))
      else if TagName = 'DPR_DataSpace' then
         add_data_region (dpr_region, int_value('beginaddr'), int_value('endaddr'))
   end;

destructor t_pic_info.Destroy;
   begin
      sfr_names.Free
   end;

INITIALIZATION

FINALIZATION
   f_pic_info.Free

END.
