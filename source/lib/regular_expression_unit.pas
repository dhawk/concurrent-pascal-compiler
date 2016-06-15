UNIT regular_expression_unit;

{$IFDEF FPC}
   {$MODE Delphi}
{$ENDIF}

INTERFACE

uses
{$IFDEF FPC}
   regexpr;
{$ELSE}
   System.RegularExpressions;
{$ENDIF}

type
   t_regular_expression =
      class
      private
{$IFDEF FPC}
         e: TRegExpr;
{$ELSE}
         regex: TRegEx;
         m: TMatch;
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
      regex := TRegEx.Create (reg_expr)
{$ENDIF}
   end;

function t_regular_expression.Matches (s: string): boolean;
   begin
{$IFDEF FPC}
      result := e.Exec(s)
{$ELSE}
      m := regex.Match(s);
      result := m.Success
{$ENDIF}
   end;

function t_regular_expression.Match (idx: integer): string;
   begin
{$IFDEF FPC}
      result := e.Match[idx]
{$ELSE}
      assert (m.Success);
      result := m.Groups[idx].Value
{$ENDIF}
   end;

destructor t_regular_expression.Destroy;
   begin
{$IFDEF FPC}
      e.Free
{$ELSE}
{$ENDIF}
   end;

END.

