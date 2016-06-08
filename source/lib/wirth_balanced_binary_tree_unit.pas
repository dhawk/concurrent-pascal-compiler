UNIT wirth_balanced_binary_tree_unit;

{$ifdef FPC}
{$MODE Delphi}
{$endif}

//
// Taken from Nicklaus Wirth :
// Algorithmen und Datenstrukturen ( in Pascal )
// Balanced Binary Trees p 250 ++
//
//
// Fixed By Giacomo Policicchio
// pgiacomo@tiscalinet.it
// 19/05/2000
//

INTERFACE

uses
   SysUtils;

type
   TBalancedTreeEntry =
      class(TObject)
         bal: -1 .. 1;
         count: integer;
         lesser_values: TBalancedTreeEntry;    // old left
         greater_values: TBalancedTreeEntry;   // old right
         constructor create;
         function compare
            (a: TBalancedTreeEntry
            ): Shortint;  // a < self :-1  a=self :0  a > self :+1
            virtual; abstract;
         procedure copy
            (ToA: TBalancedTreeEntry
            ); // data
            virtual; abstract;
      end;

   TBalancedBinaryTree =
      class
         root: TBalancedTreeEntry;
      private
         procedure Delete
            (item: TBalancedTreeEntry;
             var p: TBalancedTreeEntry;
             var h: boolean;
             var ok: boolean
            );
         procedure SearchAndInsert
            (item: TBalancedTreeEntry;
             Var p: TBalancedTreeEntry;
             var h: boolean; 
             Var Found: boolean
            );
         function SearchItem
            (item: TBalancedTreeEntry;
             Var p: TBalancedTreeEntry
            ): boolean;
         procedure balanceLeft
            (var p: TBalancedTreeEntry;
             var h: boolean; 
             dl: boolean
            );
         procedure balanceRight
            (var p: TBalancedTreeEntry;
             var h: boolean; 
             dl: boolean
            );
         Function remove
            (item: TBalancedTreeEntry
            ): boolean;
      public
         destructor Destroy;
            override;
         Function add
            (item: TBalancedTreeEntry
            ): boolean;
      end;
   ESymbolAlreadyInSymbolTable =
      class (Exception)
         entry: TBalancedTreeEntry;
         constructor Create (e: TBalancedTreeEntry);
      end;

IMPLEMENTATION

constructor ESymbolAlreadyInSymbolTable.Create (e: TBalancedTreeEntry);
   begin
      inherited Create ('item already in balance tree');
      entry := e
   end;

constructor TBalancedTreeEntry.create;
   begin
      inherited create;
      count := 0;
   end;
   
destructor TBalancedBinaryTree.Destroy;
   begin
      while root <> nil do
         remove(root);
      inherited Destroy
   end;
   
procedure TBalancedBinaryTree.SearchAndInsert
   (item: TBalancedTreeEntry;
    Var p: TBalancedTreeEntry;
    var h: boolean; 
    Var Found: boolean
   );
   begin
      Found := false;
      if p = nil then
         begin // word not in tree, insert it
            p := item;
            h := true;
            with p do
               begin
                  if root = nil then
                     root := p;
                  count := 1;
                  lesser_values := nil;
                  greater_values := nil;
                  bal := 0;
               end;
         end
      else if (item.compare(p) > 0) then // new < current
         begin
            SearchAndInsert(item, p.lesser_values, h, Found);
            if h and not Found then
               balanceLeft(p, h, false);
         end
      else if (item.compare(p) < 0) then // new > current
         begin
            SearchAndInsert(item, p.greater_values, h, Found);
            if h and not Found then
               balanceRight(p, h, false);
         end
      else  // item.compare(p) = 0!
         begin
            item.Free;
            raise ESymbolAlreadyInSymbolTable.Create (p)
         end
   end; // searchAndInsert
   
   // returns true and a pointer to the equal item if found, false otherwise
function TBalancedBinaryTree.SearchItem
   (item: TBalancedTreeEntry;
    Var p: TBalancedTreeEntry
   ): boolean;
   begin
      result := false;
      if (p = nil) then
         result := false // empty
      else
         begin
            if (item.compare(p) = 0) then
               result := true
            else if (item.compare(p) > 0) then
               result := SearchItem(item, p.lesser_values)
            else
               begin
                  if (item.compare(p) < 0) then
                     result := SearchItem(item, p.greater_values)
               end;
         end;
   end;
   
procedure TBalancedBinaryTree.balanceRight
   (var p: TBalancedTreeEntry;
    var h: boolean; 
    dl: boolean
   );
   var
      p1, p2: TBalancedTreeEntry;
   Begin
      case p.bal of
         - 1:
            begin
               p.bal := 0;
               if not dl then
                  h := false;
            end;
         0:
            begin
               p.bal := +1;
               if dl then
                  h := false;
            end;
         +1:
            begin // new balancing
               p1 := p.greater_values;
               if (p1.bal = +1) or ((p1.bal = 0) and dl) then
                  begin // single rr rotation
                     p.greater_values := p1.lesser_values;
                     p1.lesser_values := p;
                     if not dl then
                        p.bal := 0
                     else
                        begin
                           if p1.bal = 0 then
                              begin
                                 p.bal := +1;
                                 p1.bal := -1;
                                 h := false;
                              end
                           else
                              begin
                                 p.bal := 0;
                                 p1.bal := 0;
                                 (* h:=false; *)
                              end;
                        end;
                     p := p1;
                  end
               else
                  begin // double rl rotation
                     p2 := p1.lesser_values;
                     p1.lesser_values := p2.greater_values;
                     p2.greater_values := p1;
                     p.greater_values := p2.lesser_values;
                     p2.lesser_values := p;
                     if p2.bal = +1 then
                        p.bal := -1
                     else
                        p.bal := 0;
                     if p2.bal = -1 then
                        p1.bal := +1
                     else
                        p1.bal := 0;
                     p := p2;
                     if dl then
                        p2.bal := 0;
                  end;
               if not dl then
                  begin
                     p.bal := 0;
                     h := false;
                  end;
            end;
      end; // case
   End;
   
procedure TBalancedBinaryTree.balanceLeft
   (var p: TBalancedTreeEntry;
    var h: boolean; 
    dl: boolean
   );
   var
      p1, p2: TBalancedTreeEntry;
   Begin
      case p.bal of
         1:
            begin
               p.bal := 0;
               if not dl then
                  h := false;
            end;
         0:
            begin
               p.bal := -1;
               if dl then
                  h := false;
            end;
         -1: (* if (p.Left<>nil) or not dl then *)
            begin // new balancing
               p1 := p.lesser_values;
               if (p1.bal = -1) or ((p1.bal = 0) and dl) then
                  begin // single ll rotation
                     p.lesser_values := p1.greater_values;
                     p1.greater_values := p;
                     if not dl then
                        p.bal := 0
                     else
                        begin
                           if p1.bal = 0 then
                              begin
                                 p.bal := -1;
                                 p1.bal := +1;
                                 h := false;
                              end
                           else
                              begin
                                 p.bal := 0;
                                 p1.bal := 0;
                                 (* h:=false; *)
                              end;
                        end;
                     p := p1;
                  end
               else
                  begin // double lr rotation
                     p2 := p1.greater_values;
                     p1.greater_values := p2.lesser_values;
                     p2.lesser_values := p1;
                     p.lesser_values := p2.greater_values;
                     p2.greater_values := p;
                     if p2.bal = -1 then
                        p.bal := +1
                     else
                        p.bal := 0;
                     if p2.bal = +1 then
                        p1.bal := -1
                     else
                        p1.bal := 0;
                     p := p2;
                     if dl then
                        p2.bal := 0;
                  end;
               if not dl then
                  begin
                     p.bal := 0;
                     h := false;
                  end;
            end; { -1 }
      end; { case }
   End;
   
procedure TBalancedBinaryTree.Delete
   (item: TBalancedTreeEntry;
    var p: TBalancedTreeEntry;
    var h: boolean; 
    var ok: boolean
   );
   var
      q: TBalancedTreeEntry; // h=false;
      
   procedure del
      (var r: TBalancedTreeEntry;
       var h: boolean
      );
      begin // h=false
         if r.greater_values <> nil then
            begin
               del(r.greater_values, h);
               if h then
                  balanceLeft(r, h, true);
            end
         else
            begin
               r.copy(q); { q.key:=r.key; }
               q.count := r.count;
               q := r;
               r := r.lesser_values;
               h := true;
            end;
      end;
      
   begin { main of delete }
      ok := true;
      if (p = nil) then
         begin
            ok := false;
            h := false;
         end
      else if (item.compare(p) > 0) { (x < p^.key) } then
         begin
            Delete(item, p.lesser_values, h, ok);
            if h then
               balanceRight(p, h, true);
         end
      else if (item.compare(p) < 0) { (x > p^.key) } then
         begin
            Delete(item, p.greater_values, h, ok);
            if h then
               balanceLeft(p, h, true);
         end
      else
         begin // remove q
            q := p;
            if q.greater_values = nil then
               begin
                  p := q.lesser_values;
                  h := true;
               end
            else if (q.lesser_values = nil) then
               begin
                  p := q.greater_values;
                  h := true;
               end
            else
               begin
                  del(q.lesser_values, h);
                  if h then
                     balanceRight(p, h, true);
               end;
            q.free; { dispose(q) } 
            ;
         end;
   end; { delete }
   
Function TBalancedBinaryTree.add
   (item: TBalancedTreeEntry
   ): boolean;
   var
      h, Found: boolean;
   begin
      Found := false; // added to suppress compiler warning
      h := false; // added to suppress compiler warning
      SearchAndInsert(item, root, h, Found);
      add := Found;
   end;
   
Function TBalancedBinaryTree.remove
   (item: TBalancedTreeEntry
   ): boolean;
   var
      h, ok: boolean;
   begin
      ok := false; // added to suppress compiler warning
      h := false; // added to suppress compiler warning
      Delete(item, root, h, ok);
      remove := ok;
   end;
   
   
END.
