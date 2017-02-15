UNIT pic18x_information_unit;

INTERFACE

uses
  Classes, common_unit;

type
   TPICInfo = class;

   TSFRField =
      class
         fieldname: string;
         width: integer;
         bit_access: string   // only set for 1-bit fields (may be unreliable)
      end;

   TSFRMode =
      class
         id: string;
         fields: array of TSFRField;
         used: boolean;
         destructor Destroy;
            override;
      end;

   TSFRKind = (simple_sfr, joined_sfr, muxd_sfr);
   TSFR =
      class
         kind: TSFRKind;
         addr: integer;
         name: string;
         offset: integer;
         processed, ishidden: boolean;
         reversed: boolean;
         varno: string;
         function size: integer;
            virtual; abstract;
         function var_count: integer;
            virtual; abstract;
         function IsCPU_SFR: boolean;
            virtual; abstract;
      protected
         pic_info: TPICInfo;
         constructor Create (_pic_info: TPICInfo; _kind: TSFRKind);
      end;

   TSFRDef =
      class(TSFR)
         bit_access: string;  // --nnrr (should be len 8, but 9 seen) - (may be unreliable)
         modes: array of TSFRMode;
         when: string;   // used for mux'd SFRs
         function size: integer;
            override;
         constructor Create (pic_info: TPICInfo);
         function var_count: integer;
            override;
         function IsCPU_SFR: boolean;
            override;
         function IsAlternateSharedAddressSFR: boolean;
         destructor Destroy;
            override;
      end;

   TMuxdSFR =
      class(TSFR)
         name2: string;
         sfrs: array of TSFRDef;
         constructor Create (pic_info: TPICInfo);
         function IsCPU_SFR: boolean;
            override;
         function size: integer;
            override;
         function var_count: integer;
            override;
         destructor Destroy;
            override;
      end;

   TJoinedSFR =
      class(TSFR)
         sfrL, sfrH, sfrU: TSFR;
         constructor Create (pic_info: TPICInfo; _reversed: boolean);
         function size: integer;
            override;
         function var_count: integer;
            override;
         function IsCPU_SFR: boolean;
            override;
         destructor Destroy;
            override;
      end;

   TFuseByteFieldValues =
      record
         islanghidden: boolean;
         cname: string;
         desc: string;
         value: integer
      end;

   TConfigByteField =
      class
         name, desc: string;
         ishidden: boolean;
         isidehidden: boolean;
         nop_marker: boolean;
         bitno, width, default_value: integer;
         values: array of TFuseByteFieldValues;
         function empty_field: boolean;
         destructor Destroy;
            override;
      end;

   TConfigByte =
      class
         name: string;
         fields: array of TConfigByteField;
         default_value: integer;
         mode_count: integer;  // never exceeds one?
         destructor Destroy;
            override;
      end;

   TDataSpaceSectorKind = (gpr_dataspace_sector, sfr_dataspace_sector, dpr_dataspace_sector);
   TDataSpaceSector =
      class
         kind: TDataSpaceSectorKind;
         beginaddr, endaddr: integer;
         constructor Create (_kind: TDataSpaceSectorKind; _beginaddr, _endaddr: integer);
      end;

   TPICInfo =
      class (TComponent)
      public
         chip_name: string;
         config_bytes: array of TConfigByte;
         sfrs: array of TSFR;
         data_space_sectors: array of TDataSpaceSector;
         eeadr, eeadrh, eecon1, eecon2, eedata, wdtcon: integer;
         eeprom_size: integer;
         first_access_bank_absolute_address: integer;
         is_extended: boolean;
         rom_size: integer;
         procedure AppendXMLFile (out: TOutStringProc);
         destructor Destroy;
            override;
      end;

function is_cpu_reg (id: string): boolean;

IMPLEMENTATION

uses
  SysUtils, wirth_balanced_binary_tree_unit;

type
   TIDAddr =
      class (TBalancedTreeEntry)
         id: string;
         addr: integer;
         seen_in_h_file: boolean;
         constructor Create (_id: string; _addr: integer);
         function compare
            (a: TBalancedTreeEntry
            ): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy
            (ToA: TBalancedTreeEntry
            ); // data
            override;
      end;

constructor TIDAddr.Create (_id: string; _addr: integer);
   begin
      inherited Create;
      id := _id;
      addr := _addr
   end;

function TIDAddr.compare
   (a: TBalancedTreeEntry
   ): Shortint;  // a < self :-1  a=self :0  a > self :+1
   begin
      if TIDAddr(a).id < id then
         result := -1
      else if TIDAddr(a).id = id then
         result := 0
      else
         result := +1
   end;

procedure TIDAddr.copy
   (ToA: TBalancedTreeEntry
   ); // data
   begin
      TIDAddr(ToA).id := id;
      TIDAddr(ToA).addr := addr
   end;


var
   cpu_regs: TBalancedBinaryTree;  // loaded at program initialization, read-only there-after

function is_cpu_reg (id: string): boolean;

   function search (cpu_reg: TIDAddr): boolean;
      begin
         if cpu_reg = nil then
            result := false
         else if id < cpu_reg.id then
            result := search (TIDAddr (cpu_reg.lesser_values))
         else if id = cpu_reg.id then
            result := true
         else
            result := search (TIDAddr (cpu_reg.greater_values))
      end;

   begin  //is_cpu_reg
      result := search (TIDAddr(cpu_regs.root))
   end;

//===========
//  TSFRMode

destructor TSFRMode.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(fields)-1 do
         fields[i].Free;
      inherited
   end;


//=======
//  TSFR

constructor TSFR.Create (_pic_info: TPICInfo; _kind: TSFRKind);
   begin
      pic_info := _pic_info;
      kind := _kind
   end;


//==========
//  TSFRDef

constructor TSFRDef.Create (pic_info: TPICInfo);
   begin
      inherited Create (pic_info, simple_sfr)
   end;

function TSFRDef.size: integer;
   begin
      result := 1
   end;

function TSFRDef.var_count: integer;
   begin
      result := 1
   end;

function TSFRDef.IsCPU_SFR: boolean;
   begin
      result := is_cpu_reg (name)
   end;

function TSFRDef.IsAlternateSharedAddressSFR: boolean;
   const
      ADSHR_bit = 4;  // only value seen so far
   var
      i: integer;
      when_addr: integer;
   begin
      result := false;

      if (when = '')
         or
         (Pos ('($0x', when) <> 1)
      then
         exit;

      // example: when="($0xfc0 & 0x10) == 0x10"   // shows up only in alternate shared address sfr
      when_addr := 0;
      for i := 5 to 7 do
         if CharInSet (when[i], ['0'..'9']) then
            when_addr := (when_addr*16) + ord(when[i]) - ord('0')
         else if CharInSet(when[i], ['a'..'f']) then
            when_addr := (when_addr*16) + ord(when[i]) - ord('a') + 10
         else
            assert (false);

      if when_addr <> pic_info.wdtcon then
         exit;

      assert (Pos(' & 0x10) == 0x10', when) = 8);   // confirm ADSHR is bit 4 for all PIC18x's
      assert (Length(when) = 23);

      result := true
   end;

destructor TSFRDef.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(modes)-1
         do modes[i].Free;
      inherited;
   end;


//===========
//  TMuxdSFR

constructor TMuxdSFR.Create (pic_info: TPICInfo);
   begin
      inherited Create (pic_info, muxd_sfr)
   end;

function TMuxdSFR.size: integer;
   var i: integer;
   begin
      result := 0;
      for i := 0 to Length(sfrs)-1 do
         if sfrs[i].size > result then
            result := sfrs[i].size
   end;

function TMuxdSFR.var_count: integer;
   begin
      result := Length(sfrs)
   end;

function TMuxdSFR.IsCPU_SFR: boolean;
   begin
      result := false
   end;

destructor TMuxdSFR.Destroy;
   var
      i: integer;
   begin
      for i := 0 to Length(sfrs) - 1 do
         sfrs[i].Free;
      inherited
   end;


//=============
//  TJoinedSFR

constructor TJoinedSFR.Create (pic_info: TPICInfo; _reversed: boolean);
   begin
      reversed := _reversed;
      inherited Create (pic_info, joined_sfr)
   end;

function TJoinedSFR.size: integer;
   begin
      result := sfrL.size + sfrH.size
   end;

function TJoinedSFR.var_count: integer;
   begin
      assert(sfrL.kind = sfrH.kind);
      assert (sfrU = nil);
      assert (sfrL.var_count = sfrH.var_count);

      result := sfrL.var_count
   end;

function TJoinedSFR.IsCPU_SFR: boolean;
   begin
      result := is_cpu_reg (name)
   end;

destructor TJoinedSFR.Destroy;
   begin
      sfrL.Free;
      sfrH.Free;
      sfrU.Free;
      inherited
   end;


//===================
//  TConfigByteField

destructor TConfigByteField.Destroy;
   begin
      inherited
   end;

function TConfigByteField.empty_field: boolean;
   begin
      result := (name = '')
                or
                (name = '-')
                or
                ishidden
                or
                isidehidden
   end;


//===================
//  TDataSpaceSector

constructor TDataSpaceSector.Create (_kind: TDataSpaceSectorKind; _beginaddr, _endaddr: integer);
   begin
      kind := _kind;
      beginaddr := _beginaddr;
      endaddr := _endaddr
   end;

//==============
//  TConfigByte

destructor TConfigByte.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(fields)-1 do
         fields[i].Free;
      inherited
   end;


//==============
//  TPICXMLInfo

procedure TPICInfo.AppendXMLFile (out: TOutStringProc);
   var
      alt_shared_addr_sfr_test: integer;

   procedure check_alt_sfr (sfr: TSFRDef);
      const
         ADSHR = 4;  // only value seen so far
      var
         i: integer;
         when_addr: integer;
      begin
         if (sfr = nil)
            or
            (wdtcon = 0)
            or
            (sfr.when = '')
            or
            (Pos ('($0x', sfr.when) <> 1)
         then
            exit;

         // example: when="($0xfc0 & 0x10) == 0x10"   // shows up only in alternate shared address sfr
         when_addr := 0;
         for i := 5 to 7 do
            if CharInSet (sfr.when[i], ['0'..'9']) then
               when_addr := (when_addr*16) + ord(sfr.when[i]) - ord('0')
            else if CharInSet(sfr.when[i], ['a'..'f']) then
               when_addr := (when_addr*16) + ord(sfr.when[i]) - ord('a') + 10
            else
               assert (false);

         if when_addr <> wdtcon then
            exit;

         assert (Pos(' & 0x10) == 0x10', sfr.when) = 8);   // confirm ADSHR is bit 4 for all PIC18x's
         assert (Length(sfr.when) = 23);
         alt_shared_addr_sfr_test := (ADSHR shl 12) + when_addr;
      end;

   procedure out_sfr (sfr: TSFRDef);
      begin
         if (sfr <> nil)
            and
            (not is_cpu_reg (sfr.name))
         then
            if sfr.IsAlternateSharedAddressSFR then
               out (format ('      <SFR name="%s" addr="%d"/>', [sfr.name, $1000 + sfr.addr]))
            else
               out (format ('      <SFR name="%s" addr="%d"/>', [sfr.name, sfr.addr]))
      end;

   var
      sfr: TSFR;
      jsfr: TJoinedSFR;
      i: integer;
   begin
      // calculate alt_shared_addr_sfr_test
      alt_shared_addr_sfr_test := 0;
      for sfr in sfrs do
         case sfr.kind of
            simple_sfr:
               check_alt_sfr (TSFRDef(sfr));
            joined_sfr:
               begin
                  jsfr := TJoinedSFR(sfr);
                  case jsfr.sfrL.kind of
                     simple_sfr:
                        begin
                           check_alt_sfr (TSFRDef (jsfr.sfrL));
                           check_alt_sfr (TSFRDef (jsfr.sfrH));
                           check_alt_sfr (TSFRDef (jsfr.sfrU))
                        end;
                     muxd_sfr:
                        for i := 0 to jsfr.var_count-1 do
                           begin
                              assert (jsfr.sfrU = nil);
                              check_alt_sfr (TMuxdSFR(jsfr.sfrL).sfrs[i]);
                              check_alt_sfr (TMuxdSFR(jsfr.sfrH).sfrs[i])
                           end;
                  else
                     assert (false)
                  end
               end;
            muxd_sfr:
               for i := 0 to sfr.var_count-1 do
                  check_alt_sfr (TMuxdSFR(sfr).sfrs[i])
         end;

      out ('   <CPUInfo>');

      out (format ('      <Chip name="%s" is_extended="true" first_access_bank_absolute_address="%d" rom_size="%d" alt_shared_addr_test="%d"/>',
                   [chip_name, first_access_bank_absolute_address, rom_size, alt_shared_addr_sfr_test]
                  )
          );
      out (format ('      <EEPROM size="%d" eeadr="%d" eeadrh="%d" eecon1="%d" eecon2="%d" eedata="%d"/>',
                   [eeprom_size, eeadr, eeadrh, eecon1, eecon2, eedata]
                  )
          );

      for sfr in sfrs do
         case sfr.kind of
            simple_sfr:
               out_sfr (TSFRDef(sfr));
            joined_sfr:
               begin
                  jsfr := TJoinedSFR(sfr);
                  case jsfr.sfrL.kind of
                     simple_sfr:
                        begin
                           out_sfr (TSFRDef (jsfr.sfrL));
                           out_sfr (TSFRDef (jsfr.sfrH));
                           out_sfr (TSFRDef (jsfr.sfrU))
                        end;
                     muxd_sfr:
                        for i := 0 to jsfr.var_count-1 do
                           begin
                              assert (jsfr.sfrU = nil);
                              out_sfr (TMuxdSFR(jsfr.sfrL).sfrs[i]);
                              out_sfr (TMuxdSFR(jsfr.sfrH).sfrs[i])
                           end;
                  else
                     assert (false)
                  end
               end;
            muxd_sfr:
               for i := 0 to sfr.var_count-1 do
                  out_sfr (TMuxdSFR(sfr).sfrs[i])
         end;
      out ('   </CPUInfo>')
   end;

destructor TPICInfo.Destroy;
   var i: integer;
   begin
      for i := 0 to Length(config_bytes)-1
         do config_bytes[i].Free;
      for i := 0 to Length(sfrs)-1
         do sfrs[i].Free;
      for i := 0 to Length(data_space_sectors)-1
         do data_space_sectors[i].Free;
      inherited
   end;

procedure init_cpu_register_list (reg: string; addr: integer);
   begin
      cpu_regs.add (TIDAddr.Create (reg, addr))
   end;


INITIALIZATION
   cpu_regs := TBalancedBinaryTree.Create;
   init_cpu_register_list ('BSR', $FE0);
   init_cpu_register_list ('EEADR', -1);
   init_cpu_register_list ('EEADRH', -1);
   init_cpu_register_list ('EECON1', -1);
   init_cpu_register_list ('EECON2', -1);
   init_cpu_register_list ('EEDATA', -1);
   init_cpu_register_list ('FSR0', $FE9);
   init_cpu_register_list ('FSR0H', $FEA);
   init_cpu_register_list ('FSR0L', $FE9);
   init_cpu_register_list ('FSR1', $FE1);
   init_cpu_register_list ('FSR1H', $FE2);
   init_cpu_register_list ('FSR1L', $FE1);
   init_cpu_register_list ('FSR2', $FD9);
   init_cpu_register_list ('FSR2H', $FDA);
   init_cpu_register_list ('FSR2L', $FD9);
   init_cpu_register_list ('INDF0', $FEF);
   init_cpu_register_list ('INDF1', $FE7);
   init_cpu_register_list ('INDF2', $FDF);
   init_cpu_register_list ('PCL', $FF9);
   init_cpu_register_list ('PCLAT', $FF9);
   init_cpu_register_list ('PCLATH', $FFA);
   init_cpu_register_list ('PCLATU', $FFB);
   init_cpu_register_list ('PLUSW0', $FEB);
   init_cpu_register_list ('PLUSW1', $FE3);
   init_cpu_register_list ('PLUSW2', $FDB);
   init_cpu_register_list ('POSTDEC0', $FED);
   init_cpu_register_list ('POSTDEC1', $FE5);
   init_cpu_register_list ('POSTDEC2', $FDD);
   init_cpu_register_list ('POSTINC0', $FEE);
   init_cpu_register_list ('POSTINC1', $FE6);
   init_cpu_register_list ('POSTINC2', $FDE);
   init_cpu_register_list ('PREINC0', $FEC);
   init_cpu_register_list ('PREINC1', $FE4);
   init_cpu_register_list ('PREINC2', $FDC);
   init_cpu_register_list ('PROD', $FF3);
   init_cpu_register_list ('PRODH', $FF4);
   init_cpu_register_list ('PRODL', $FF3);
   init_cpu_register_list ('STATUS', $FD8);
   init_cpu_register_list ('STKPTR', $FFC);
   init_cpu_register_list ('TABLAT', $FF5);
   init_cpu_register_list ('TBLPTR', $FF6);
   init_cpu_register_list ('TBLPTRH', $FF7);
   init_cpu_register_list ('TBLPTRL', $FF6);
   init_cpu_register_list ('TBLPTRU', $FF8);
   init_cpu_register_list ('TOS', $FFD);
   init_cpu_register_list ('TOSH', $FFE);
   init_cpu_register_list ('TOSL', $FFD);
   init_cpu_register_list ('TOSU', $FFF);
   init_cpu_register_list ('WREG', $FE8);

FINALIZATION
   cpu_regs.Free;

END.

