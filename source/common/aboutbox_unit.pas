UNIT aboutbox_unit;

INTERFACE

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, ExtCtrls, wirth_balanced_binary_tree_unit;

type
   TAboutBoxForm =
      class(TForm)
         BuilderLabel1: TLabel;
         BuilderLabel2: TLabel;
         BuilderLabel3: TLabel;
         BuiltUsingGroupBox: TGroupBox;
         Label1: TLabel;
         Label2: TLabel;
         Label3: TLabel;
         Memo1: TMemo;
         OKButton: TButton;
         ProductName: TLabel;
         WebsiteLabel: TLabel;
         procedure FormCreate(Sender: TObject);
         procedure OKButtonClick(Sender: TObject);
      private
         built_using_image: TImage;
         cpc_logo_image: TImage;
         credits: TBalancedBinaryTree;
      protected
         procedure AddThirdPartyCredit (s: string);
         procedure AddWirthCredit;
         procedure AddFastMM4Credit;
         procedure AddCommonThirdPartyCredits;
         procedure AddThirdPartyCredits;
            virtual; abstract;
      public
         destructor Destroy;
            override;
      end;

IMPLEMENTATION

{$R *.dfm}

{$IFDEF FPC}
uses
   LazarusVersionSupport;
{$ENDIF}


//===========
//  tCredit
//===========

type
   tCredit =
      class (TBalancedTreeEntry)
         string_list: TStringList;
         constructor Create (s: string);
         function compare (a: TBalancedTreeEntry): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy (ToA: TBalancedTreeEntry); // data
            override;
         destructor Destroy;
            override;
      end;

constructor tCredit.Create (s: string);
   var
      ss: string;
      idx, i: integer;
   begin
      string_list := TStringList.Create;
      idx := Pos ('|', s);
      while idx > 0 do
         begin
            ss := s;
            SetLength(ss, idx-1);
            string_list.Add (ss);
            for i := idx+1 to Length(s) do
               s[i-idx] := s[i];
            SetLength (s, Length(s)-idx);
            idx :=  Pos ('|', s)
         end;
      string_list.Add (s)
   end;

function tCredit.compare (a: TBalancedTreeEntry): Shortint;  // a < self :-1  a=self :0  a > self :+1
   begin
      result := CompareText (tCredit(a).string_list[0], string_list[0])
   end;

procedure tCredit.copy(ToA: TBalancedTreeEntry);
   begin
      tCredit(ToA).string_list.Text := string_list.Text
   end;

destructor tCredit.Destroy;
   begin
      string_list.Free;
      inherited
   end;


{$IFNDEF FPC}
function DelphiVersion: string;
   begin
      result := 'Delphi ???';  // default if no other define works
{$IFDEF VER310}
      result := 'Delphi 10.1 Berlin';
{$ENDIF}
{$IFDEF VER300}
      result := 'Delphi 10 Seattle';
{$ENDIF}
{$IFDEF VER290}
      result := 'Delphi XE8';
{$ENDIF}
{$IFDEF VER280}
      result := 'Delphi XE7';
{$ENDIF}
{$IFDEF VER270}
      result := 'Delphi XE6';
{$ENDIF}
{$IFDEF VER260}
      result := 'Delphi XE5';
{$ENDIF}
{$IFDEF VER250}
      result := 'Delphi XE4';
{$ENDIF}
{$IFDEF VER240}
         result := 'Delphi XE3';
{$ENDIF}
{$IFDEF VER230}
      result := 'Delphi XE2';
{$ENDIF}
{$IFDEF VER220}
      result := 'Delphi XE';
{$ENDIF}
{$IFDEF VER210}
      result := 'Delphi 2010';
{$ENDIF}
{$IFDEF VER200}
      result := 'Delphi 2009';
{$ENDIF}
{$IFDEF VER190}
      result := 'Delphi 2007.NET';
{$ENDIF}
{$IFDEF VER180}       // Process VER180 before VER185 since Delphi 2007 defines both
      result := 'Delphi 2006';
{$ENDIF}
{$IFDEF VER185}
      result := 'Delphi 2007';
{$ENDIF}
{$IFDEF VER170}
      result := 'Delphi 2005';
{$ENDIF}
{$IFDEF VER160}
      result := 'Delphi 8.NET';
{$ENDIF}
{$IFDEF VER150}
      result := 'Delphi 7';
{$ENDIF}
{$IFDEF VER140}
      result := 'Delphi 6';
{$ENDIF}
{$IFDEF VER130}
      result := 'Delphi 5';
{$ENDIF}
{$IFDEF VER120}
      result := 'Delphi 4';
{$ENDIF}
{$IFDEF VER100}
      result := 'Delphi 3';
{$ENDIF}
{$IFDEF VER90}
     result := 'Delphi 2';
{$ENDIF}
{$IFDEF VER80}
      result := 'Delphi 1';
{$ENDIF}
   end;
{$ENDIF}

procedure TAboutBoxForm.FormCreate(Sender: TObject);
{$IFDEF FPC}
   procedure adj_top (c: TControl);
      begin
         c.Top := c.Top - 12
      end;
{$ENDIF}
   procedure sort_and_append_credits_to_memo (cr: tCredit);
      begin
         if cr.lesser_values <> nil then
            sort_and_append_credits_to_memo (tCredit(cr.lesser_values));
         Memo1.Lines.AddStrings (cr.string_list);
         Memo1.Lines.Add ('------------------------------------------------------------');
         if cr.greater_values <> nil then
            sort_and_append_credits_to_memo (tCredit(cr.greater_values))
      end;
   begin
      cpc_logo_image := TImage.Create(Self);
      with cpc_logo_image do
         begin
            Name := 'CPCLogoImage';
            Parent := Self;
            Left := 8;
            Top := 8;
            Width := 129;
            Height := 129;
         end;
      built_using_image := TImage.Create(Self);
      with built_using_image do
         begin
            Name := 'BuiltUsingImage';
            Parent := BuiltUsingGroupBox;
            Left := 13;
            Top := 20;
            Width := 60;
            Height := 60
         end;
      credits := TBalancedBinaryTree.Create;
      cpc_logo_image.Picture.Bitmap.LoadFromResourceName(HInstance, 'CP_LOGO');
{$IFDEF FPC}
      BuiltUsingGroupBox.Caption := 'Built with Lazrus/Free Pascal';
      adj_top (built_using_image);
      adj_top (BuilderLabel1);
      built_using_image.Picture.Bitmap.LoadFromResourceName(HInstance, 'LAZARUS_LOGO');
      BuilderLabel1.caption := GetLCLVersion;
      adj_top (BuilderLabel2);
      BuilderLabel2.Caption := 'Compiled with ' + GetCompilerInfo;
      adj_top (BuilderLabel3);
      BuilderLabel3.caption := GetWidgetSet;
{$ELSE}
      BuiltUsingGroupBox.Caption := 'Built with Delphi';
      built_using_image.Picture.Bitmap.LoadFromResourceName (HInstance, 'DELPHI_LOGO');
      BuilderLabel1.Caption := '';
      BuilderLabel2.Caption := 'Compiled with ' + DelphiVersion;
      BuilderLabel3.Caption := '';
{$ENDIF}
      AddThirdPartyCredits;
      sort_and_append_credits_to_memo (tCredit(credits.root));
      Memo1.Lines.Delete (Memo1.Lines.Count-1)   // delete last -------------
   end;

procedure TAboutBoxForm.OKButtonClick(Sender: TObject);
   begin
      Close
   end;

procedure TAboutBoxForm.AddWirthCredit;
   begin
      AddThirdPartyCredit ('Balanced Binary Trees|Nicklaus Wirth|Algorithmen und Datenstrukturen, p. 250|Fixed By Giacomo Policicchio|pgiacomo@tiscalinet.it');
   end;

procedure TAboutBoxForm.AddFastMM4Credit;
   begin
{$IFNDEF FPC}
      AddThirdPartyCredit ('FastMM4 - Fast Memory Manager|Pierre le Riche|https://github.com/pleriche/FastMM4');
{$ENDIF}
   end;

procedure TAboutBoxForm.AddCommonThirdPartyCredits;
   begin
      AddThirdPartyCredit ('SmoothSort|Edsger Dijkstra|http://en.wikibooks.org/wiki/Algorithm_Implemenation/Sorting/Smoothsort');
      AddThirdPartyCredit ('LibXmlParser|Stefan Heymann|www.destructor.de');
      AddThirdPartyCredit ('MPArith - multi precision integer arithmetic|Wolfgang Ehrhardt|http://wolfgang-ehrhardt.de');
      AddWirthCredit;
      AddFastMM4Credit
   end;

procedure TAboutBoxForm.AddThirdPartyCredit (s: string);
   begin
      credits.Add (tCredit.Create (s))
   end;

destructor TAboutBoxForm.Destroy;
   begin
      credits.Free;
      inherited
   end;

END.
