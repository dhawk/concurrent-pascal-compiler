unit about_credit_frame_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, wirth_balanced_binary_tree_unit,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
   TAboutCreditFrame =
      class(TFrame)
         Panel1: TPanel;
    BuiltUsingGroupBox: TGroupBox;
         BuiltUsingImage: TImage;
         BuilderLabel1: TLabel;
         BuilderLabel2: TLabel;
         BuilderLabel3: TLabel;
         Label1: TLabel;
         Memo1: TMemo;
      private
         credits: TBalancedBinaryTree;
      public
         procedure Init;
         procedure AddThirdPartyCredit (s: string);
         procedure SetThirdParyCredits;
         destructor Destroy;
            override;
      end;

implementation

{$R *.dfm}

{$IFDEF FPC}
uses
   LazarusVersionSupport;
{$ENDIF}

type
   tCredit =
      class (TBalancedTreeEntry)
         string_list: TStringList;
         constructor Create (s: string);
         function compare
            (a: TBalancedTreeEntry
            ): Shortint;  // a < self :-1  a=self :0  a > self :+1
            override;
         procedure copy
            (ToA: TBalancedTreeEntry
            ); // data
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

function tCredit.compare
   (a: TBalancedTreeEntry
   ): Shortint;  // a < self :-1  a=self :0  a > self :+1
   begin
      result := CompareText (tCredit(a).string_list[0], string_list[0])
   end;

procedure tCredit.copy
   (ToA: TBalancedTreeEntry
   ); // data
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
      result := 'Delphi 10+';  // default if no other define works
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

procedure TAboutCreditFrame.Init;
{$IFDEF FPC}
   procedure adj_top (c: TControl);
      begin
         c.Top := c.Top - 12
      end;
{$ENDIF}
   begin
      credits := TBalancedBinaryTree.Create;
{$IFDEF FPC}
      BuiltUsingGroupBox.Caption := 'Built with Lazrus/Free Pascal';
      adj_top (BuiltUsingImage);
      BuiltUsingImage.Picture.LoadFromFile (ExtractFilePath(ParamStr(0)) + 'images' + PathDelim + 'lazarus48x48.png');
      adj_top (BuilderLabel1);
      BuilderLabel1.caption := GetLCLVersion;
      adj_top (BuilderLabel2);
      BuilderLabel2.Caption := 'Compiled with ' + GetCompilerInfo;
      adj_top (BuilderLabel3);
      BuilderLabel3.caption := GetWidgetSet;
{$ELSE}
      BuiltUsingGroupBox.Caption := 'Built with Delphi';
      BuiltUsingImage.Picture.LoadFromFile (ExtractFilePath(ParamStr(0)) + 'images' + PathDelim + 'delphi52x52.gif');
      BuilderLabel1.Caption := '';
      BuilderLabel2.Caption := 'Compiled with ' + DelphiVersion;
      BuilderLabel3.Caption := '';
{$ENDIF}
      AddThirdPartyCredit ('SmoothSort|Edsger Dijkstra|http://en.wikibooks.org/wiki/Algorithm_Implemenation/Sorting/Smoothsort');
      AddThirdPartyCredit ('Balanced Binary Trees|Nicklaus Wirth|Algorithmen und Datenstrukturen, p. 250|Fixed By Giacomo Policicchio|pgiacomo@tiscalinet.it');
      AddThirdPartyCredit ('FastMM4 - Fast Memory Manager|Pierre le Riche|https://github.com/pleriche/FastMM4');
      AddThirdPartyCredit ('LibXmlParser|Stefan Heymann|www.destructor.de');
      AddThirdPartyCredit ('MPArith - multi precision integer arithmetic|Wolfgang Ehrhardt|http://wolfgang-ehrhardt.de');
      SetThirdParyCredits
   end;

procedure TAboutCreditFrame.AddThirdPartyCredit (s: string);
   begin
      credits.Add (tCredit.Create (s))
   end;

procedure TAboutCreditFrame.SetThirdParyCredits;
   procedure append_credits (cr: tCredit);
      begin
         if cr.lesser_values <> nil then
            append_credits (tCredit(cr.lesser_values));
         Memo1.Lines.AddStrings (cr.string_list);
         Memo1.Lines.Add ('-----------------------------------------------------------------');
         if cr.greater_values <> nil then
            append_credits (tCredit(cr.greater_values))
      end;
   begin
      Memo1.Clear;
      append_credits (tCredit(credits.root));
      Memo1.Lines.Delete (Memo1.Lines.Count-1)   // delete last -------------
   end;

destructor TAboutCreditFrame.Destroy;
   begin
      credits.Free;
      inherited
   end;

end.
