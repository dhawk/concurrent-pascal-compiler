UNIT regular_expression_unit;

// This unit defines an interface for a limited regular expression matcher that allows subexpressions to be extracted.
// Depending on the compiler environment the following regular expression libraries are used to implement it:
//    - Delphi XE and above - the builtin RegularExpressions unit.
//    - pre-XE Delphi - PerlRegEx
//    - Lazarus - the regexpr library

{$IFDEF FPC}
   {$MODE Delphi}
{$ELSE}
   {$if CompilerVersion < 22.0}   // XE was first with reg expressions built in
      {$define USE_PERLREGEX}
   {$ifend}
{$ENDIF}

INTERFACE

uses
{$IFDEF FPC}
   regexpr;
{$ELSE}
   {$IFDEF USE_PERLREGEX}
   PerlRegEx;
   {$ELSE}
   RegularExpressions;
   {$ENDIF}
{$ENDIF}

type
   t_regular_expression =
      class
      private
{$IFDEF FPC}
         e: TRegExpr;
{$ELSE}
   {$IFDEF USE_PERLREGEX}
         PerlRegEx: TPerlRegEx;
   {$ELSE}
         regex: TRegEx;
         m: TMatch;
   {$ENDIF}
{$ENDIF}
      public
         constructor Create (reg_expr: string);
         function Matches (s: string): boolean;
         function Match (idx: integer): string;
         destructor Destroy;
            override;
      end;


IMPLEMENTATION

constructor t_regular_expression.Create (reg_expr: string);
   begin
{$IFDEF FPC}
      e := TRegExpr.Create;
      e.Expression := reg_expr;
{$ELSE}
   {$IFDEF USE_PERLREGEX}
      PerlRegEx := TPerlRegEx.Create;
      PerlRegEx.RegEx := reg_expr
   {$ELSE}
      regex := TRegEx.Create (reg_expr)
   {$ENDIF}
{$ENDIF}
   end;

function t_regular_expression.Matches (s: string): boolean;
   begin
{$IFDEF FPC}
      result := e.Exec(s)
{$ELSE}
   {$IFDEF USE_PERLREGEX}
      PerlRegEx.Subject := s;
      result := PerlRegEx.Match
   {$ELSE}
      m := regex.Match(s);
      result := m.Success
   {$ENDIF}
{$ENDIF}
   end;

function t_regular_expression.Match (idx: integer): string;
   begin
{$IFDEF FPC}
      result := e.Match[idx]
{$ELSE}
   {$IFDEF USE_PERLREGEX}
      assert (PerlRegEx.Match);
      result := PerlRegEx.Groups[idx];
   {$ELSE}
      assert (m.Success);
      result := m.Groups[idx].Value
   {$ENDIF}
{$ENDIF}
   end;

destructor t_regular_expression.Destroy;
   begin
{$IFDEF FPC}
      e.Free
{$ELSE}
   {$IFDEF USE_PERLREGEX}
      PerlRegEx.Free
   {$ELSE}
   {$ENDIF}
{$ENDIF}
   end;

END.

