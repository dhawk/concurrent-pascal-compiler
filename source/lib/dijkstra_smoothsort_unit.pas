
unit dijkstra_smoothsort_unit;
//  Delphi implementation of Dijkstra's algorithm
//  downloaded from http://en.wikibooks.org/wiki/Algorithm_Implementation/Sorting/Smoothsort on 5/18/2012
//  minor modification to take an array of objects and comparison function, drh

interface

type
   TSmoothSortArray = array of TObject;
   TSmoothSortComparisonFunction = function (v1,v2: TObject): boolean;
      //   result := v1 <= v2
 
procedure SmoothSort(var A: TSmoothSortArray; IsAscending: TSmoothSortComparisonFunction);
 
implementation
 
{ implementation of Djikstra's algorithm }
procedure SmoothSort(var A: TSmoothSortArray; IsAscending: TSmoothSortComparisonFunction);
var q,r,
    p,b,c,
    r1,b1,c1,
    N: integer;
 
 procedure up(var vb,vc: integer);
 var temp: integer;
 begin
    temp := vb;
    vb := vb+vc+1;
    vc := temp;
 end;
 
 procedure down(var vb,vc: integer);
 var temp: integer;
 begin
   temp := vc;
   vc := vb-vc-1;
   vb := temp;
 end;
 
 procedure sift;
 var r0, r2: integer;
     T: TObject;
 begin
   r0 := r1;
   T := A[r0];
   while b1>=3 do
   begin
     r2 := r1-b1+c1;
     if not IsAscending(A[r1-1], A[r2]) then
     begin
       r2 := r1-1;
       down(b1,c1);
     end;
     if IsAscending(A[r2], T) then b1 := 1 else
     begin
       A[r1] := A[r2];
       r1 := r2;
       down(b1,c1);
     end;
   end;
   if r1<>r0 then A[r1] := T;
 end;
 
 procedure trinkle;
 var p1,r2,r3, r0 : integer;
     T: TObject;
 begin
    p1 := p; b1 := b; c1 := c;
    r0 := r1;
    T := A[r0];
    while p1>0 do
    begin
      while (p1 and 1)=0 do
      begin
        p1 := p1 shr 1; up(b1,c1);
      end;
      r3 := r1-b1;
      if (p1=1) or IsAscending(A[r3], T) then p1 := 0 else
      {p1>1}
      begin
        p1 := p1 - 1;
        if b1=1 then
        begin
          A[r1] := A[r3];
          r1 := r3;
        end else
        if b1>=3 then
        begin
           r2 := r1-b1+c1;
           if not IsAscending(A[r1-1], A[r2]) then
           begin
             r2 := r1-1;
             down(b1,c1); p1 := p1 shl 1;
           end;
           if IsAscending(A[r2], A[r3]) then
           begin
             A[r1] := A[r3];
             r1 := r3;
           end else
           begin
             A[r1] := A[r2];
             r1 := r2;
             down(b1,c1); p1 := 0;
           end;
        end;{if b1>=3}
      end;{if p1>1}
    end;{while}
    if r0<>r1 then A[r1] := T;
    sift;
 end;
 
 procedure semitrinkle;
 var T: TObject;
 begin
   r1 := r-c;
   if not IsAscending(A[r1], A[r]) then
   begin
     T := A[r];
     A[r] := A[r1];
     A[r1] := T;
     trinkle;
   end;
 end;
 
begin
   N := length(A);
   if N = 0 then exit;
   q := 1; r := 0;
   p := 1; b := 1; c := 1;
 
   //building tree
   while q < N do
   begin
     r1 := r;
     if (p and 7) = 3 then
     begin
        b1 := b; c1 := c; sift;
        p := (p+1) shr 2; up(b,c); up(b,c);
     end
     else if (p and 3) = 1 then
     begin
       if q + c < N then
       begin
          b1 := b; c1 := c; sift;
       end else trinkle;
       down(b,c); p := p shl 1;
       while b > 1 do
       begin
         down(b,c); p := p shl 1;
       end;
       p := p+1;
     end;
     q := q + 1; r := r + 1;
   end;
   r1 := r; trinkle;
 
   //bulding sorted array
   while q>1 do
   begin
     q := q - 1;
     if b = 1 then
     begin
       r := r - 1;
       p := p - 1;
       while (p and 1) = 0 do
       begin
         p := p shr 1; up(b,c);
       end;
     end else
     if b >= 3 then
     begin
       p := p - 1; r := r-b+c;
       if p > 0 then semitrinkle;
       down(b,c); p := p shl 1 + 1;
       r := r+c; semitrinkle;
       down(b,c); p := p shl 1 + 1;
     end;
     //element q is done
   end;
   //element 0 is done
end;
 
end.