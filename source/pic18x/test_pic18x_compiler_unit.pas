UNIT test_pic18x_compiler_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

INTERFACE

procedure RunTests;

IMPLEMENTATION

uses
   test_pic18x_compiler_main_form_unit, pic18x_run_time_error_check_unit,
   pic18x_instructions_unit, pic18x_statements_unit, SysUtils;

procedure test1;
   // constant assignment and sign extension for constants
   begin
      add ('var i: int8;');
      add (' i1, i2, i3: int16;');
      add (' i4: int24;');
      add ('begin i := 5;');
      add ('i1 := 5;');
      add ('i2 := 0;');
      add ('i3 := -5;');
      add ('i4 := -260;');
      add ('END.');
      start_test (1);
      test_abs_int_value (0, 1, 5);
      test_abs_int_value (1, 2, 5);
      test_abs_int_value (3, 2, 0);
      test_abs_int_value (5, 2, -5);
      test_abs_int_value (7, 3, -260);
      conclude_test
   end;

procedure test2;
   // variable assignment and sign extension for variables
   begin
      add ('var i: int8; j1, j2, j3: int16;');
      add ('begin i := 5; j1 := i; ');
      add ('i := 0; j2 := i;');
      add ('i := -5; j3 := i END.');
      start_test (2);
      test_abs_value (1, 0, 5);
      test_abs_value (3, 0, 0);
      test_abs_value (5, $ff, $fb);
      conclude_test
   end;

procedure test3;
   // rom constant assignments
   begin
      add ('rom r: record i: int8; j: int16 end = (i=5, j=6);');
      add ('var i: int8; j: int16;');
      add ('begin i := r.i; j := r.j END.');
      start_test(3);
      test_abs_value (0, 5);
      test_abs_value (1, 0, 6);
      conclude_test
   end;

procedure test4;
   // copy assignemnt, rom and var
   begin
      add ('type tr=record i: int8; j: int16 end;');
      add ('rom r: tr = (i=5, j=6);');
      add ('var v1, v2: tr;');  // 0,3
      add ('begin v1 := r; v2 := v1 END.');
      start_test(4);
      test_abs_value (0, 5);
      test_abs_value (1, 0, 6);
      test_abs_value (3, 5);
      test_abs_value (4, 0, 6);
      conclude_test
   end;

// range check integer tests
procedure test5;
   begin
      add ('var i1: int8; i2: int16; i3: int24; b: int24;');
      add ('begin');
      add ('b := 127;');
      add ('i1 := b;');
      add ('i2 := b;');
      add ('i3 := b;');
      add ('END.');
      start_test(5);
      test_abs_value (0, 127);
      test_abs_value (1, 0, 127);
      test_abs_value (3, 0, 0, 127);
      conclude_test
   end;

procedure test6;
   begin
      add ('var i1: int8; i2: int16; i3: int24; b: int24;');
      add ('begin');
      add ('b := -128;');
      add ('i1 := b;');
      add ('i2 := b;');
      add ('i3 := b;');
      add ('END.');
      start_test(6);
      test_abs_value (0, $80);
      test_abs_value (1, $ff, $80);
      test_abs_value (3, $ff, $ff, $80);
      conclude_test
   end;

procedure test7;
   begin
      add ('var i1: int8; i2: int16; i3: int24; b: int24;');
      add ('begin');
      add ('b := 128;');
      add ('i1 := b;');
      add ('i2 := b;');
      add ('i3 := b;');
      add ('END.');
      start_test(7);
      test_abs_value (0, 0);
      test_abs_value (1, 0, 128);
      test_abs_value (3, 0, 0, 128);
      conclude_test
   end;

procedure test8;
   begin
      add ('var i1: int8; i2: int16; i3: int24; b: int24;');
      add ('begin');
      add ('b := -129;');
      add ('i1 := b;');
      add ('i2 := b;');
      add ('i3 := b;');
      add ('END.');
      start_test(8);
      test_abs_value (0, 0);
      test_abs_value (1, $ff, $7f);
      test_abs_value (3, $ff, $ff, $7f);
      conclude_test
   end;

procedure test9;
   begin
      add ('var u1,u2: uint8; i: int8;');
      add ('    u3,u4: uint16; i2: int16;');
      add ('begin');
      add ('i := 127; u1 := i;');
      add ('i := -128; u2 := i;');
      add ('i2 := 32767; u3 := i2;');
      add ('i2 := -32768; u4 := i2;');
      add ('END.');
      start_test(9);
      test_abs_value (0, 127);
      test_abs_value (1, 0);
      test_abs_value (3, $7f, $ff);
      test_abs_value (5, 0, 0);
      conclude_test
   end;

procedure test10;
   begin
      add ('var u1,u2,u3: uint8; i: int24;');
      add ('begin');
      add ('i := 255; u1 := i;');
      add ('i := -128; u2 := i;');
      add ('i := 256; u3 := i;');
      add ('END.');
      start_test(10);
      test_abs_value (0, 255);
      test_abs_value (1, 0);
      test_abs_value (2, 0);
      conclude_test
   end;

procedure test11;
   begin
      add ('var i1,i2,i3: int8; u: uint8;');
      add ('begin');
      add ('u := 255; i1 := u;');
      add ('u := 127; i2 := u;');
      add ('u := 0; i3 := u;');
      add ('END.');
      start_test(11);
      test_abs_value (0, 0);
      test_abs_value (1, 127);
      test_abs_value (2, 0);
      conclude_test
   end;

procedure test12;
   begin
      add ('var i1,i2,i3,i4: int8;');
      add (' u: int16;');
      add ('begin');
      add ('u := 128; i1 := u;');
      add ('u := 127; i2 := u;');
      add ('u := -128; i3 := u;');
      add ('u := -129; i4 := u');
      add ('END.');
      start_test(12);
      test_abs_value (0, 0);
      test_abs_value (1, 127);
      test_abs_value (2, $80);
      test_abs_value(3, 0);
      conclude_test
   end;

procedure test13;
   begin
      add ('var i1,i2: int16; i3,i4: int8;');
      add (' u: -100_000..-1 := -1;');
      add ('begin');
      add ('u := -32769; i1 := u;');
      add ('u := -32768; i2 := u;');
      add ('u := -129; i3 := u;');
      add ('u := -128; i4 := u');
      add ('END.');
      start_test(13);
      test_abs_value (0, 0, 0);
      test_abs_value (2, $80, 0);
      test_abs_value (4, 0);
      test_abs_value (5, $80);
      conclude_test
   end;

procedure test14;
   begin
      add ('var i1,i2: int16; i3,i4: int8;');
      add (' u: uint24;');
      add ('begin');
      add ('u := 32768; i1 := u;');
      add ('u := 32767; i2 := u;');
      add ('u := 128; i3 := u;');
      add ('u := 127; i4 := u');
      add ('END.');
      start_test(14);
      test_abs_value (0, 0, 0);
      test_abs_value (2, $7f, $ff);
      test_abs_value (4, 0);
      test_abs_value (5, 127);
      conclude_test
   end;

procedure test15;
   begin
      add ('var i1,i2,i3,i4: 0..3;');
      add ('var ii1,ii2,ii3,ii4: 0..300;');
      add ('var i5,i6: 0..3;');
      add (' u: uint16; i: int16; ii: -1..3;');
      add ('begin');
      add ('i := -1; i1 := i;');
      add ('u := 0; i2 := u;');
      add ('u := 3; i3 := u;');
      add ('u := 4; i4 := u;');
      add ('i := -1; ii1 := i;');
      add ('u := 0; ii2 := u;');
      add ('u := 300; ii3 := u;');
      add ('u := 301; ii4 := u;');
      add ('ii := 2; i5 := ii;');
      add ('ii := -1; i6 := ii;');
      add ('END.');
      start_test(15);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 3);
      test_abs_value (3, 0);
      test_abs_value (4, 0, 0);
      test_abs_value (6, 0, 0);
      test_abs_value (8, 1, $2c);
      test_abs_value (10, 0, 0);
      test_abs_value (12, 2);
      test_abs_value (13, 0);
      conclude_test
   end;

procedure test16;
   // upper bound is -1
   begin
      add ('var i1,i2,i3,i4: -3..-1 := -2;');
      add ('var ii1,ii2,ii3,ii4: -300..-1 := -2;');
      add (' i5,i6: -3..-1 := -2;');
      add (' i: int8; u: -500..-1:=-2;');
      add (' ii: -5..-2 := -3;');
      add ('begin');
      add ('i := 0; i1 := i;');
      add ('u := -1; i2 := u;');
      add ('u := -3; i3 := u;');
      add ('u := -4; i4 := u;');
      add ('i := 0; ii1 := i;');
      add ('u := -1; ii2 := u;');
      add ('u := -300; ii3 := u;');
      add ('u := -301; ii4 := u;');
      add ('ii := -3; i5 := ii;');
      add ('ii := -4; i6 := ii;');
      add ('END.');
      start_test(16);
      test_abs_value (0, byte(-1));
      test_abs_value (1, $ff);
      test_abs_value (2, $fd);
      test_abs_value (3, byte(-1));
      test_abs_value (4, byte(-1), byte(-1));
      test_abs_value (6, $ff, $ff);
      test_abs_value (8, $fe, $d4);
      test_abs_value (10, byte(-1), byte(-1));
      test_abs_value (12, $fd);
      test_abs_value (13, byte(-1));
      conclude_test
   end;

procedure test17;
   // both tos and assignee same signed
   begin
      add ('var i1,i2,i3,i4: 5..7 := 6;');
      add (' i5,i6,i7,i8: -600..-500 := -550;');
      add (' i9,i10,i11,i12: 5..7 := 6;');
      add (' i: uint8; ii: -1000..-400 := -555;');
      add (' x: 4..6 := 5; y: 6..8 := 7;');
      add ('begin');
      add (' i:=8; i1:=i;');
      add (' i:=7; i2:=i;');
      add (' i:=5; i3:=i;');
      add (' i:=4; i4:=i;');
      add (' ii:=-601; i5:=ii;');
      add (' ii:=-600; i6:=ii;');
      add (' ii:=-500; i7:=ii;');
      add (' ii:=-499; i8:=ii;');
      add ('x:=4; i9 := x;');
      add ('x:=5; i10 := x;');
      add ('y:=7; i11:=y;');
      add ('y:=8; i12 := y;');
      add ('END.');
      start_test (17);
      test_abs_value (0, 5);
      test_abs_value (1, 7);
      test_abs_value (2, 5);
      test_abs_value (3, 5);
      test_abs_value (4, $fd, $a8);
      test_abs_value (6, $fd, $a8);
      test_abs_value (8, $fe, $0c);
      test_abs_value (10, $fd, $a8);

      test_abs_value (12, 5);
      test_abs_value (13, 5);
      test_abs_value (14, 7);
      test_abs_value (15, 5);
      conclude_test
   end;

procedure test18;
   begin
      add ('var i1,i2,i3,i4: -5..6;');
      add (' i5,i6,i7,i8: -500..600;');
      add (' i12,i15,i18,i21: -70000..80000;');
      add (' i24,i25,i26,i27: -5..6;');
      add (' i: int8; ii: int16; iii: int24;');
      add (' x: -10..0; y: 0..10;');
      add ('begin');
      add (' i:=7; i1:=i;');
      add (' i:=6; i2:=i;');
      add (' i:=-5; i3:=i;');
      add (' i:=-6; i4:=i;');
      add (' ii:=601; i5:=ii;');
      add (' ii:=600; i6:=ii;');
      add (' ii:=-500; i7:=ii;');
      add (' ii:=-501; i8:=ii;');
      add (' iii:=80001; i12:=iii;');
      add (' iii:=80000; i15:=iii;');
      add (' iii:=-70000; i18:=iii;');
      add (' iii:=-70001; i21:=iii;');
      add (' x := -6; i24 := x;');
      add (' x := -5; i25 := x;');
      add (' y := 6; i26 := y;');
      add (' y := 7; i27 := y;');
      add ('END.');
      start_test (18);
      test_abs_value (0, 0);
      test_abs_value (1, 6);
      test_abs_value (2, $fb);
      test_abs_value (3, 0);
      test_abs_value (4, 0, 0);
      test_abs_value (6, 2, $58);
      test_abs_value (8, $fe, $0c);
      test_abs_value (10, 0, 0);
      test_abs_value (12, 0, 0, 0);
      test_abs_value (15, 1, $38, $80);
      test_abs_value (18, $fe, $ee, $90);
      test_abs_value (21, 0, 0, 0);
      test_abs_value (24, 0);
      test_abs_value (25, byte(-5));
      test_abs_value (26, 6);
      test_abs_value (27, 0);
      conclude_test
   end;

procedure test19;
   begin
      add ('var a: int8;');   // 0
      add (' b: int16;');  // 1
      add (' c: int16; ');   // 3
      add (' d: int8;');   // 5
      add (' e: int16;');  // 6
      add (' f: int24;');   // 8
      add (' i8, i8b: int8; i16: int16; i24: int24;');
      add (' p: 30000..30010:=30000;');
      add (' m: -30010..-30000:=-30010;');
      add ('begin');
      add (' i8b := 5; i8 := 6; a := i8b + i8;');
      add (' b := 255; i16 := 100; b := b + i16;');
      add (' i24 := 1234; i16 := 555; c := i24 + i16;');
      add (' d := p + m;');
      add (' i8:=100; i16 := 500; e := i8 + i16;');
      add ('i8:=100; i16 := 200; i8b := 50; f := i8 - i16 + i8b;');
      add ('END.');
      start_test (19);
      test_abs_int_value (0, 1, 11);
      test_abs_int_value (1, 2, 255 + 100);
      test_abs_int_value (3, 2, 1234+555);
      test_abs_int_value (5, 1, 30000-30010);
      test_abs_int_value (6, 2, 100+500);
      test_abs_int_value (8, 3, 100-200+50);
      conclude_test
   end;

procedure test20;
   begin
      add ('var a: int8;');   // 0
      add (' b: int16;');  // 1
      add (' c: int16; ');   // 3
      add (' d: int8;');   // 5
      add (' e: int16;');  // 6
      add (' f,f2: int24;');   // 8,11
      add (' i8, i8b: int8; i16: int16; i24: int24;');
      add (' p: -30010..-30000:=-30000;');
      add (' m: -30010..-30000:=-30010;');
      add ('begin');
      add (' a := 5; i8 := 6; a := a - i8;');
      add (' b := 255; i16 := 100; b := b - i16;');
      add (' i24 := 1234; i16 := 555; c := i24 - i16;');
      add (' d := p - m;');
      add (' i8:=100; i16 := 500; e := i8 - i16;');
      add ('i8:=-100; i16 := -200; i8b := -50; f := i8 + i16 - i8b;');
      add ('f2 := i8 + i16;');
      add ('');
      add ('END.');
      start_test (20);
      test_abs_int_value (0, 1, -1);
      test_abs_int_value (1, 2, 255 - 100);
      test_abs_int_value (3, 2, 1234-555);
      test_abs_int_value (5, 1, -30000+30010);
      test_abs_int_value (6, 2, 100-500);
      test_abs_int_value (8, 3, -100-200+50);
      test_abs_int_value (11, 3, -100-200);
      conclude_test
   end;

procedure test21;   // boolean or
   begin
      add ('var b1,b2,b3,b4: boolean;');   // 0,1,2,3
      add (' t1,t2: boolean;');
      add ('begin');
      add (' t1:=false; t2:=false; b1 := t1 or t2;');
      add (' t1:=false; t2:=true; b2 := t1 or t2;');
      add (' t1:=true; t2:=false; b3 := t1 or t2;');
      add (' t1:=true; t2:=true; b4 := t1 or t2;');
      add ('END.');
      start_test (21);
      test_abs_value (0, 0);
      test_abs_value (1, 1);
      test_abs_value (2, 1);
      test_abs_value (3, 1);
      conclude_test
   end;

procedure test22;  // set + and -
   begin
      add ('var s1,s2,s3,s4: set of 0..23;');   // 0,3,6,9
      add (' t1,t2: set of 0..15;');
      add ('begin');
      add ('  t1:=[0,1,2]; t2 := [3,4,5]; s1 := t1 + t2;');
      add ('  t1:=[0..7]; t2 := [0,9]; s2 := t1 - t2;');
      add ('  t1:=[0,9]; s3 := t1 + [23];');
      add ('END.');
      start_test (22);
      test_abs_value (0, 0, 0, $3f);
      test_abs_value (3, 0, 0, $fe);
      test_abs_value (6, $80, $02, $01);
      conclude_test
   end;

procedure test23;        // multiply
   begin
      add ('var i: 0..10;');   // 0
      add (' j: 0..1000;');    // 1
      add (' k: 0..100_000;'); // 3
      add (' l1,l2: int16;');  // 6,8
      add (' i1,i2,i3,i4: int16;');  // 10,12,14,16
      add ('begin');
      add ('  i := 3; i := i * i;');
      add ('  j := i*i;');
      add ('  k := j*j;');
      add ('  l2 := -2; l1 := k * l2;');    // unsigned * signed
      add ('  l2 := 2; l2 := l2 * l1;');  // signed * signed
      add ('  i1:=-2;i2:=-2;i1 := i1*i2;');  //- -
      add ('  i2 := 2; i3:=-2; i2 := i3*i2;'); // - +
      add ('  i3 := i1*i2;');  // + -
      add ('END.');
      start_test (23);
      test_abs_value (0, 9);
      test_abs_value (1, 0, 81);
      test_abs_value (3, 0, $19, $a1);
      test_abs_value (6, $cc, $be);
      test_abs_value (8, $99, $7c);
      test_abs_value (10, 0, 4);
      test_abs_value (12, $ff, $fc);
      test_abs_value (14, $ff, $f0);
      conclude_test
   end;

procedure test24;      // div
   begin
      add ('var i: 0..10;');   // 0
      add (' j: 0..1000;');    // 1
      add (' k: 0..100_000;'); // 3
      add (' i1,i2,i3,i4,i5: int16;'); // 6,8,10,12,14
      add (' t1,t2: int8;');
      add (' t3: -64..0; t4: 0..2;');
      add ('begin');
      add ('  k := 10000; j := 50; k := k div j;');
      add ('  i := 10; j := 100; j := j div i;');
      add ('  i := 3; i := i div i;');
      add ('  t1 := -128; t2:=1; i1 := t1 div t2;');  // - +
      add ('  t1:=-128; t2:=-1; i2 := t1 div t2;'); // - -
      add ('  t1:=127; t2:=-1; i3 := t1 div t2;');  // + -
      add ('  t1:=127; t2:=1; i4 := t1 div t2;');  // + +
      add ('  t3:=-64; t4:=2; i5:=1; i5:=t3*t4 div i5;');
      add ('END.');
      start_test (24);
      test_abs_value (0, 1);
      test_abs_value (1, 0, 10);
      test_abs_value (3, 0, 0, 200);
      test_abs_value (6, $ff, $80);
      test_abs_value (8,  0, $80);
      test_abs_value (10, $ff, $81);
      test_abs_value (12,  0, $7f);
      test_abs_value (14, $ff, $80);
      conclude_test
   end;

procedure test25;     // mod
   begin
      add ('var i: 0..10;');   // 0
      add (' j: 0..1000;');    // 1
      add (' k,k2: uint32;'); // 3!,7!
      add (' m: uint40;');  // 11
      add (' mm: uint24;');  // 16!
      add (' la: uint24;');  // 19
      add (' lb: uint40;');  // 22!
      add (' x1,x2: -16..16;');  //  27!,28!
      add ('begin');
      add ('  k := 15_781_936; k2 := 74_565; k := k mod k2;');
      add ('  m:= 4886718345; mm := 74565; mm := m mod mm;');
      add ('  la:= 74565; lb := 4886718345; lb := la mod lb;');  // + mod +
      add ('  x1 := 16; x2 := -16; x1 := x1 * x2 mod x1;');      // - mod -
      add ('  k2 := k2 mod x2;');   // + mod -
      add ('  x2 := x2 mod 7;');    // - mod +
      add ('END.');
      start_test (25);
      test_abs_value (3, 0, 0, $be, $51);
      test_abs_value (16, 0, $67, $89);
      test_abs_value (22, 0, 0, $1, $23, $45);
      test_abs_value (27, 0);
      test_abs_value (7, 0, 0, 0, 5);
      test_abs_value (28, $fe);
      conclude_test
   end;

procedure test26;   // boolean and
   begin
      add ('var b1,b2,b3,b4,b5: boolean;');   // 0,1,2,3,4
      add (' t1,t2: boolean;');
      add ('begin');
      add (' t1:=false; t2:=false; b1 := t1 and t2;');
      add (' t1:=false; t2:=true; b2 := t1 and t2;');
      add (' t1:=true; t2:=false; b3 := t1 and t2;');
      add (' t1:=true; t2:=true; b4 := t1 and t2;');
      add (' b5 := t1 and t2 and b1;');
      add ('END.');
      start_test (26);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 0);
      test_abs_value (3, 1);
      test_abs_value (4, 0);
      conclude_test
   end;

procedure test27;   // set intersection
   begin
      add ('var s1,s2,s3,s4,s5: set of 0..15;');   // 0,2,4,6,8
      add (' t1,t2: set of 0..15;');
      add ('begin');
      add (' t1 := [0..7]; t2 := [0..3]; s1 := t1 * t2;');
      add (' t1 := [8]; t2 := [7..9]; s2 := t1 * t2;');
      add ('END.');
      start_test (27);
      test_abs_value (0, 0, $0f);
      test_abs_value (2, $01, $00);
      conclude_test
   end;

procedure test28;   // div by pwr of 2 constant
   begin
      add ('var a1,a2: int8;'); // 0,1
      add (' b1,b2,b3,b4: int16;');   // 2,4,6,8
      add ('begin');
      add (' a1 := 120; a1 := a1 div 2;');
      add (' a2 := -120; a2 := a2 div 2;');
      add (' b1 := 200; b1 := b1 div 4;');
      add (' b2 := -200; b2 := b2 div 4;');
      add (' b3 := 640; b3 := b3 div 16;');
      add (' b4 := -640; b4 := b4 div 32;');
      add ('END.');
      start_test (28);
      test_abs_value (0, 60);
      test_abs_value (1, $c4);
      test_abs_value (2, 0, 50);
      test_abs_value (4, $ff, $ce);
      test_abs_value (6, 0, 40);
      test_abs_value (8, $ff, $ec);
      conclude_test
   end;

procedure test29;   // relational expr
   begin
      add ('var b0,b1,b2: boolean;');
      add ('    b3,b4,b5: boolean;');
      add ('    b6,b7,b8: boolean;');
      add ('    b9,b10,b11: boolean;');
      add ('    b12,b13,b14: boolean;');
      add ('    b15,b16,b17: boolean;');
      add ('    b18,b19: boolean;');
      add (' i: int16;');
      add (' s: set of 0..15;');
      add ('begin');
      add (' i := 3; b0 := i < 4; b1 := i < 3; b2 := i < 2;');
      add (' b3 := i=4; b4 := i=3; b5:= i=2;');
      add (' b6 := i<>4; b7 := i<>3; b8 := i <> 2;');
      add (' b9:= i>=4; b10:= i>=3; b11:= i>=2;');
      add (' b12:= i<=4; b13:= i<=3; b14:= i<=2;');
      add (' b15:= i>4; b16:= i>3; b17:= i>2;');
      add (' s := [14]; i := 14; b18:=i in s;');
      add (' i := 7; b19 := i in s;');
      add ('END.');
      start_test (29);
      test_abs_value (0, 1);
      test_abs_value (1, 0);
      test_abs_value (2, 0);
      test_abs_value (3, 0);
      test_abs_value (4, 1);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      test_abs_value (7, 0);
      test_abs_value (8, 1);
      test_abs_value (9, 0);
      test_abs_value (10, 1);
      test_abs_value (11, 1);
      test_abs_value (12, 1);
      test_abs_value (13, 1);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      test_abs_value (16, 0);
      test_abs_value (17, 1);
      test_abs_value (18, 1);
      test_abs_value (19, 0);
      conclude_test
   end;

procedure test30;   // set builder
   begin
      add ('var');
      add (' s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: set of 0..15;');  // 0,2,4,6,8,10,12,14,16,18
      add (' i1: int8;');
      add (' i2: int16;');
      add (' u1: uint8;');
      add (' u2: uint16;');
      add (' u15: 0..15; u23: 0..23; u31: 0..31;');
      add ('begin');
      add (' u1 := 1; s1 := [u1];');
      add (' u2 := 2; s2 := [u2];');
      add (' i1 := 3; s3 := [i1];');
      add (' i2 := 4; s4 := [i2];');
      add (' i1 := -1; s5 := [i1];');
      add (' i2 := -1; s6 := [i2];');
      add (' i1 := 16; s7 := [i1];');
      add (' u15:=15; u23 := 23; u31 := 31; s8 := [u15,u23,u31];');
      add (' i2:= 256; s9 := [i2];');
      add (' u2:= 256; s10 := [u2];');
      add ('END.');
      start_test (30);
      test_abs_value (0, 0, 2);
      test_abs_value (2, 0, 4);
      test_abs_value (4, 0, 8);
      test_abs_value (6, 0, $10);
      test_abs_value (8, 0, 0);
      test_abs_value (10, 0, 0);
      test_abs_value (12, 0, 0);
      test_abs_value (14, $80, 0);
      test_abs_value (16, 0, 0);
      test_abs_value (18, 0, 0);
      conclude_test
   end;

procedure test31;   // set builder
   begin
      add ('var');
      add (' s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: set of 0..15;');  // 0,2,4,6,8,10,12,14,16,18
      add (' z1: int8; s11: set of 0..15; z2: int8;');  // 20,21,23
      add (' i1,i2: 0..15;');
      add (' i3,i4: int16;');
      add ('begin');
      add ('  i1:=0; i2:=15; s1:=[i1..i2]; s2 := [i2..i1];');
      add ('  i1:=0; i2:=0; s3:=[i1..i2]; s4 := [i2..i1];');
      add ('  i3:=0; i4:=15; s5:=[i3..i4]; s6 := [i4..i3];');
      add ('  i3:=0; i4:=0; s7:=[i3..i4]; s8 := [i4..i3];');
      add ('  z1:=0;z2:=0; i3:=-1; i4:=16; s11:=[i3..i4]; s9:=[i4..i3];');
      add ('END.');
      start_test (31);
      test_abs_value (0, $ff, $ff);
      test_abs_value (2, 0, 0);
      test_abs_value (4, 0, 1);
      test_abs_value (6, 0, 1);
      test_abs_value (8, $ff, $ff);
      test_abs_value (10, 0, 0);
      test_abs_value (12, 0, 1);
      test_abs_value (14, 0, 1);
      test_abs_value (16, 0, 0);
      test_abs_value (20, 0, $ff, $ff, 0);
      conclude_test
   end;

procedure test32;   // procedure call & non-indexed parameter/local vars
   begin
      add ('type tr=record i: int8; j: int16 end;');   // 3 bytes
      add ('type ta=array[-1..2] of int16;');  // 8 bytes
      add ('procedure p1; begin end;');
      add ('procedure p2 (var i: int8; var j: int16); begin i := 5; j := 6 end;');
      add ('procedure p3 (v: tr; var r: tr);');
      add (' var l: tr;');
      add ('  begin');
      add ('     l.j := v.j;');
      add ('     r.j := l.j');
      add (' end;');
      add ('procedure p4 (v: ta; var r: ta);');
      add ('  var l: ta;');
      add ('  begin');
      add ('     l[0] := v[1];');
      add ('     r[2] := l[0]');
      add ('  end;');
      add ('var i: int8; j: int16;');   // 0, 2
      add (' r1,r2: tr;');  // 3,6
      add (' a1,a2: ta;');  // 9, 17
      add ('begin');
      add ('  p1;');
      add ('  p2(i,j);');
      add ('  r1.j := 23;');
      add (' p3(r1,r2);');
      add (' a1[1]:=5; p4(a1,a2);');
      add ('END.');
      start_test (32);
      test_abs_value (0, 5);
      test_abs_value (2, 6);
      test_abs_value (3, 0, 0, 23);
      test_abs_value (6, 0, 0, 23);
      test_abs_value (17+6, 0, 5);
      conclude_test
   end;

procedure test33;   // zero divide checks
   begin
      add ('var err1: uint24;');  // 0
      add ('  i8a: int8;');       // 3
      add (' i16a: int16;');      // 4
      add (' err2: uint24;');     // 6
      add (' m8a: -100..0;');     // 9
      add (' err3: uint24;');     // 10
      add (' m16a: -100..0;');    // 13
      add (' i8c: int8;');         // 14
      add (' i16c: int16;');      // 15
      add (' err4: uint24;'); // 17
      add (' i8d: int8;');   //20
      add (' err5: uint24;'); // 21
      add (' i8b: int8; i16b: int16;');
      add ('begin');
      add ('  i8b := 7; i8a := i8b div i8a; err1 := ErrorCode;');
      add ('  i16b := $6666; i16a := i16b div i16a; err2 := ErrorCode;');
      add (' i8b := 72; i8c := i8b div m8a; err3 := ErrorCode;');
      add (' i16b := 1234; i16c := i16b div m16a; err4 := ErrorCode;');
      add (' i8b:=7; i8d := i8b mod i8d; err5 := ErrorCode;');
      add ('end.');
      start_test (33);
      test_abs_value (3, $f9);          // i8a
      test_run_time_error_detected (0, rterr_attempted_divide_by_zero);
      test_abs_value (4, $66, $66);         // i16a
      test_run_time_error_detected (6, rterr_attempted_divide_by_zero);
      test_abs_value (14, $b8);         // i8c
      test_run_time_error_detected (10, rterr_attempted_divide_by_zero);
      test_abs_value (15, $fb, $2e);          // ic16c
      test_run_time_error_detected (17, rterr_attempted_divide_by_zero);
      test_abs_value (20, 0);
      test_run_time_error_detected (21, rterr_attempted_divide_by_zero);
      conclude_test
   end;

procedure test34;
   begin
      add ('type');
      add ('  tr = record i,j: int16 end;');
      add ('  tc =');
      add ('     class (i: int16; r: tr);');
      add ('        var j: int16;');
      add ('     public');
      add ('        procedure p1 (var x: int16);');
      add ('          begin');
      add ('             x := j');
      add ('          end;');
      add ('        procedure p2 (ii: int16);');
      add ('          begin');
      add ('             j := ii');
      add ('          end;');
      add ('        procedure p3 (var kk: int16);');
      add ('          begin');
      add ('             kk := r.j');
      add ('          end;');
      add ('        begin');
      add ('           j := i;');
      add ('        end;');
      add ('var');
      add ('   i,j,k: int16;');  // 0,2,4
      add ('   c: tc;');
      add ('   r: tr;');
      add ('begin');
      add ('   r.i := 10;');
      add ('   r.j := 11;');
      add ('   init c(6,r);');
      add ('   c.p1(i);');
      add ('   c.p2(7);');
      add ('   c.p1(j);');
      add ('   c.p3(k)');
      add ('end.');
      start_test (34);
      test_abs_value (0, 0, 6);
      test_abs_value (2, 0, 7);
      test_abs_value (4, 0, 11);
      conclude_test
   end;

procedure test35;
   begin
      add ('procedure p (var i: int16);');
      add ('   begin');
      add ('      i := 5');
      add ('   end;');
      add ('procedure p2 (var v: int16);');
      add ('   var i: int16;');
      add ('   begin');
      add ('      p (i);');
      add ('      v := i');
      add ('   end;');
      add ('var i: int16;');
      add ('begin');
      add ('   p2 (i)');
      add ('end.');
      start_test (35);
      test_abs_value (0, 0, 5);
      conclude_test
   end;

procedure test36;    // variable array bounds, simple and comples
   begin
      add ('type');
      add ('  rng = 3..4;');
      add ('  t=');
      add ('   array [rng] of');
      add ('      record');
      add ('          i,j: int16');
      add ('       end;');
      add ('rom');
      add ('  r: t = ([3]=(i=5,j=6),');
      add ('          [4]=(i=7,j=8)');
      add ('         );');
      add ('function f1 (rom x: t): int16;');
      add (' var b: rng;');
      add (' begin');
      add ('   b := 3;');
      add ('   result := x[b].j');
      add (' end;');
      add ('function f2 (rom x: t): int16;');
      add (' var b: int8;');
      add (' begin');
      add ('   b := 3;');
      add ('   result := x[b].j');
      add (' end;');
      add ('var');
      add ('   i,j,k: int16;  // 0,2,4');
      add ('   ii,jj,kk: int16;  // 6,8,10');
      add ('   cv: t := ([3]=(i=15,j=16),');
      add ('         [4]=(i=17,j=18));');
      add ('   b: rng;');
      add ('   u: int8;');
      add ('begin');
      add ('   b := 4;');
      add ('   i := r[b].j;');
      add ('   b := 3;');
      add ('   j := cv[b].i;');
      add ('   k := f1(r);');
      add ('   u := 4;');
      add ('   ii := r[u].j;');
      add ('   u := 3;');
      add ('   jj := cv[u].i;');
      add ('   kk := f2(r)');
      add ('end.');
      start_test (36);
      test_abs_value (0, 0, 8);
      test_abs_value (2, 0, 15);
      test_abs_value (4, 0, 6);
      test_abs_value (6, 0, 8);
      test_abs_value (8, 0, 15);
      test_abs_value (10, 0, 6);
      conclude_test
   end;

procedure test37;    // equals and <>
   begin
      add ('var');
      add (' b1,b2,b3,b4,b5,b6,b7,b8: boolean;');
      add (' i3: int24;');
      add (' i2: int16;');
      add (' i1a,i1b: int8;');
      add ('begin');
      add (' i3:=4000;');
      add (' i2 := 4000;');
      add (' b1 := i3=i2;');
      add (' b2 := i3 <> i2;');
      add (' i2 := 3000;');
      add (' b3 := i3=i2;');
      add (' b4 := i3 <> i2;');
      add (' i1a:=-7; i1b:=-7; b5 := i1a = i1b; b6 := i1a <> i1b;');
      add (' i1b:=43; b7 := i1a = i1b; b8 := i1a <> i1b;');
      add ('end.');
      start_test (37);
      test_abs_value (0, 1);
      test_abs_value (1, 0);
      test_abs_value (2, 0);
      test_abs_value (3, 1);
      test_abs_value (4, 1);
      test_abs_value (5, 0);
      test_abs_value (6, 0);
      test_abs_value (7, 1);
      conclude_test
   end;

procedure test38;    // if, loop
   begin
      add ('var');
      add ('   i0,i2,i4,i6: int16;');
      add ('   i: int24;');
      add ('begin');
      add ('   i := 7;');
      add ('   if i = 6 then');
      add ('      i0 := 5');
      add ('   else if i = 7 then');
      add ('      i2 := 8;');
      add ('   loop');
      add ('      i4 := i4 + 1;');
      add ('      i := i + 1;');
      add ('      until i = 10');
      add ('   repeat;');
      add ('   loop');
      add ('      while i > 3;');
      add ('      i6 := i6 - 1;');
      add ('      i := i - 1');
      add ('   repeat;');
      add ('   cycle');
      add ('   repeat');
      add ('end.');
      start_test (38);
      test_abs_value (0, 0, 0);
      test_abs_value (2, 0, 8);
      test_abs_value (4, 0, 3);
      test_abs_value (6, $ff, $f9);
      conclude_test
   end;

procedure test39;    // for to - unsigned byte control var
   begin
      add ('procedure p (lo,hi: uint8; var c: int16);');
      add ('  var i: uint8;');
      add ('  begin');
      add ('    for i := lo to hi do');
      add ('       c := c + 1');
      add ('  end;');
      add ('var');
      add ('   i0,i2,i4,i6,i8: int16;');
      add ('   i10,i12,i14,i16,i18: int16;');
      add ('   i20,i22,i24,i26,i28: int16;');
      add ('   u,u1,u2: uint8;');
      add ('   uu, uu1, uu2: 0..20;');
      add ('begin');
      // constant loop limits
      add ('   for u := 6 to 5 do');
      add ('      i0 := i0 + 1;');
      add ('   for u := 128 to 0 do');
      add ('      i2 := i2 + 1;');
      add ('   for u := 4 to 6 do');
      add ('      i4 := i4 + 1;');
      add ('   for u := 120 to 130 do');
      add ('      i6 := i6 + 1;');
      add ('   for u := 0 to 255 do');
      add ('      i8 := i8 + 1;');
      add ('   for u := 12 to 12 do');
      add ('      i10 := i10 + 1;');
      // test variable limits, local control var
      add ('   p (6,5,i12);');
      add ('   p (128,0,i14);');
      add ('   p (4,6,i16);');
      add ('   p (120,130,i18);');
      add ('   p (0,255,i20);');
      add ('   p (12,12,i22);');
      add ('   uu1:=1;uu2:=10;');
      add ('   for uu := uu1 to uu2 do');
      add ('      i24:=i24+1;');
      add ('   for uu := uu1 to 10 do');
      add ('      i26 := i26+1;');
      add ('end.');
      start_test (39);
      test_abs_value (0, 0, 0);
      test_abs_value (2, 0, 0);
      test_abs_value (4, 0, 3);
      test_abs_value (6, 0, 11);
      test_abs_value (8, 1, 0);
      test_abs_value (10, 0, 1);
      test_abs_value (12, 0, 0);
      test_abs_value (14, 0, 0);
      test_abs_value (16, 0, 3);
      test_abs_value (18, 0, 11);
      test_abs_value (20, 1, 0);
      test_abs_value (22, 0, 1);
      test_abs_value (24, 0, 10);
      test_abs_value (26, 0, 10);
      conclude_test
   end;

procedure test40;    // for to - signed byte control var
   begin
      add ('procedure p (lo,hi: int8; var c: int16);');
      add ('  var i: int8;');
      add ('  begin');
      add ('    for i := lo to hi do');
      add ('       c := c + 1');
      add ('  end;');
      add ('var');
      add ('   i0,i2,i4,i6,i8: int16;');
      add ('   i10,i12,i14,i16,i18: int16;');
      add ('   i20,i22,i24,i26,i28: int16;');
      add ('   i: int8;');
      add ('begin');
      // constant loop limits, global control var
      add ('   for i := 6 to 5 do');
      add ('      i0 := i0 + 1;');
      add ('   for i := 127 to -128 do');
      add ('      i2 := i2 + 1;');
      add ('   for i := 4 to 6 do');
      add ('      i4 := i4 + 1;');
      add ('   for i := -5 to 5 do');
      add ('      i6 := i6 + 1;');
      add ('   for i := -128 to 127 do');
      add ('      i8 := i8 + 1;');
      add ('   for i := 127 to 127 do');
      add ('      i10 := i10 + 1;');
      // test variable limits, local control var
      add ('   p (6,5,i12);');
      add ('   p (127,-128,i14);');
      add ('   p (4,6,i16);');
      add ('   p (-5,5,i18);');
      add ('   p (-128,127,i20);');
      add ('   p (127,127,i22);');
      add ('end.');
      start_test (40);
      test_abs_value (0, 0, 0);
      test_abs_value (2, 0, 0);
      test_abs_value (4, 0, 3);
      test_abs_value (6, 0, 11);
      test_abs_value (8, 1, 0);
      test_abs_value (10, 0, 1);
      test_abs_value (12, 0, 0);
      test_abs_value (14, 0, 0);
      test_abs_value (16, 0, 3);
      test_abs_value (18, 0, 11);
      test_abs_value (20, 1, 0);
      test_abs_value (22, 0, 1);
      conclude_test
   end;

procedure test41;    // for downto - unsigned byte control var
   begin
      add ('procedure p (lo,hi: uint8; var c: int16);');
      add ('  var i: uint8;');
      add ('  begin');
      add ('    for i := lo downto hi do');
      add ('       c := c + 1');
      add ('  end;');
      add ('var');
      add ('   i0,i2,i4,i6,i8: int16;');
      add ('   i10,i12,i14,i16,i18: int16;');
      add ('   i20,i22,i24,i26,i28: int16;');
      add ('   u: uint8;');
      add ('begin');
      // constant loop limits
      add ('   for u := 5 downto 6 do');
      add ('      i0 := i0 + 1;');
      add ('   for u := 0 downto 128 do');
      add ('      i2 := i2 + 1;');
      add ('   for u := 6 downto 4 do');
      add ('      i4 := i4 + 1;');
      add ('   for u := 130 downto 120 do');
      add ('      i6 := i6 + 1;');
      add ('   for u := 255 downto 0 do');
      add ('      i8 := i8 + 1;');
      add ('   for u := 12 downto 12 do');
      add ('      i10 := i10 + 1;');
      // test variable limits, local control var
      add ('   p (5,6,i12);');
      add ('   p (0,128,i14);');
      add ('   p (6,4,i16);');
      add ('   p (130,120,i18);');
      add ('   p (255,0,i20);');
      add ('   p (12,12,i22);');
      add ('end.');
      start_test (41);
      test_abs_value (0, 0, 0);
      test_abs_value (2, 0, 0);
      test_abs_value (4, 0, 3);
      test_abs_value (6, 0, 11);
      test_abs_value (8, 1, 0);
      test_abs_value (10, 0, 1);
      test_abs_value (12, 0, 0);
      test_abs_value (14, 0, 0);
      test_abs_value (16, 0, 3);
      test_abs_value (18, 0, 11);
      test_abs_value (20, 1, 0);
      test_abs_value (22, 0, 1);
      conclude_test
   end;

procedure test42;    // for downto - signed byte control var
   begin
      add ('procedure p (lo,hi: int8; var c: int16);');
      add ('  var i: int8;');
      add ('  begin');
      add ('    for i := lo downto hi do');
      add ('       c := c + 1');
      add ('  end;');
      add ('var');
      add ('   i0,i2,i4,i6,i8: int16;');
      add ('   i10,i12,i14,i16,i18: int16;');
      add ('   i20,i22,i24,i26,i28: int16;');
      add ('   i: int8;');
      add ('begin');
      // constant loop limits, global control var
      add ('   for i := 5 downto 6 do');
      add ('      i0 := i0 + 1;');
      add ('   for i := -128 downto 127 do');
      add ('      i2 := i2 + 1;');
      add ('   for i := 6 downto 4 do');
      add ('      i4 := i4 + 1;');
      add ('   for i := 5 downto -5 do');
      add ('      i6 := i6 + 1;');
      add ('   for i := 127 downto -128 do');
      add ('      i8 := i8 + 1;');
      add ('   for i := 127 downto 127 do');
      add ('      i10 := i10 + 1;');
      // test variable limits, local control var
      add ('   p (5,6,i12);');
      add ('   p (-128,127,i14);');
      add ('   p (6,4,i16);');
      add ('   p (5,-5,i18);');
      add ('   p (127,-128,i20);');
      add ('   p (127,127,i22);');
      add ('   i := 10;');
      add ('   for i := i downto 1 do');
      add ('      i24 := i24 + 1;');
      add ('end.');
      start_test (42);
      test_abs_value (0, 0, 0);
      test_abs_value (2, 0, 0);
      test_abs_value (4, 0, 3);
      test_abs_value (6, 0, 11);
      test_abs_value (8, 1, 0);
      test_abs_value (10, 0, 1);
      test_abs_value (12, 0, 0);
      test_abs_value (14, 0, 0);
      test_abs_value (16, 0, 3);
      test_abs_value (18, 0, 11);
      test_abs_value (20, 1, 0);
      test_abs_value (22, 0, 1);
      test_abs_value (24, 0, 10);
      conclude_test
   end;

procedure test43;    // for to - unsigned multi-byte control var
   begin
      add ('var');
      add ('  c0,c2,c4,c6,c8,c10,c12,c14: uint16;');
      add ('procedure test (var counter: uint16; first, last: uint16);');
      add ('  var');
      add ('     u: uint16;');
      add ('  begin');
      add ('     for u := first to last do');
      add ('       counter := counter + 1');
      add ('  end;');
      add ('begin');
      add ('   test (c0, 1, 10);');
      add ('   test (c2, 255, 255);');
      add ('   test (c4, 256, 256);');
      add ('   test (c6, 0, 255);');
      add ('   test (c8, 1, 1000);');
      add ('   test (c10, 256, 255);');
      add ('end.');
      start_test (43);
      test_abs_value (0, 0, 10);
      test_abs_value (2, 0, 1);
      test_abs_value (4, 0, 1);
      test_abs_value (6, 1, 0);
      test_abs_value (8, 3, $e8);
      test_abs_value (10, 0, 0);
      conclude_test
   end;

procedure test44;    // for to - signed multi-byte control var
   begin
      add ('var');
      add ('  c0,c2,c4,c6,c8,c10,c12,c14: uint16;');
      add ('procedure test (var counter: uint16; first, last: int16);');
      add ('  var');
      add ('     i: int16;');
      add ('  begin');
      add ('     for i := first to last do');
      add ('       counter := counter + 1');
      add ('  end;');
      add ('begin');
      add ('   test (c0, 1, 10);');
      add ('   test (c2, 255, 255);');
      add ('   test (c4, 256, 256);');
      add ('   test (c6, 0, 255);');
      add ('   test (c8, 1, 1000);');
      add ('   test (c10, 256, 255);');
      add ('   test (c12, -1000, 1000);');
      add ('   test (c14, 1000, -1000);');
      add ('end.');
      start_test (44);
      test_abs_value (0, 0, 10);
      test_abs_value (2, 0, 1);
      test_abs_value (4, 0, 1);
      test_abs_value (6, 1, 0);
      test_abs_value (8, 3, $e8);
      test_abs_value (10, 0, 0);
      test_abs_value (12, 7, $d1);
      test_abs_value (14, 0, 0);
      conclude_test
   end;

procedure test45;    // for downto - unsigned multi-byte control var
   begin
      add ('var');
      add ('  c0,c2,c4,c6,c8,c10,c12,c14: uint16;');
      add ('procedure test (var counter: uint16; first, last: uint16);');
      add ('  var');
      add ('     u: uint16;');
      add ('  begin');
      add ('     for u := first downto last do');
      add ('       counter := counter + 1');
      add ('  end;');
      add ('begin');
      add ('   test (c0, 10, 1);');
      add ('   test (c2, 255, 255);');
      add ('   test (c4, 256, 256);');
      add ('   test (c6, 255, 0);');
      add ('   test (c8, 1000, 1);');
      add ('   test (c10, 255, 256);');
      add ('end.');
      start_test (45);
      test_abs_value (0, 0, 10);
      test_abs_value (2, 0, 1);
      test_abs_value (4, 0, 1);
      test_abs_value (6, 1, 0);
      test_abs_value (8, 3, $e8);
      test_abs_value (10, 0, 0);
      conclude_test
   end;

procedure test46;    // for downto - signed multi-byte control var
   begin
      add ('var');
      add ('  c0,c2,c4,c6,c8,c10,c12,c14: uint16;');
      add ('procedure test (var counter: uint16; first, last: int16);');
      add ('  var');
      add ('     i: int16;');
      add ('  begin');
      add ('     for i := first downto last do');
      add ('       counter := counter + 1');
      add ('  end;');
      add ('begin');
      add ('   test (c0, 10, 1);');
      add ('   test (c2, 255, 255);');
      add ('   test (c4, 256, 256);');
      add ('   test (c6, 255, 0);');
      add ('   test (c8, 1000, 1);');
      add ('   test (c10, 255, 256);');
      add ('   test (c12, 1000, -1000);');
      add ('   test (c14, -1000, 1000);');
      add ('end.');
      start_test (46);
      test_abs_value (0, 0, 10);
      test_abs_value (2, 0, 1);
      test_abs_value (4, 0, 1);
      test_abs_value (6, 1, 0);
      test_abs_value (8, 3, $e8);
      test_abs_value (10, 0, 0);
      test_abs_value (12, 7, $d1);
      test_abs_value (14, 0, 0);
      conclude_test
   end;

procedure test47;    // case - unsigned single byte selector
   begin
      add ('procedure p (idx: uint8; var result: int8);');
      add ('   begin');
      add ('      case idx of');
      add ('         1: result := 101;');
      add ('         2: result := 102;');
      add ('         3..5: result := 105;');
      add ('         7: result := 107;');
      add ('         9..10: result := 110;');
      add ('      otherwise');
      add ('         result := 66');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   arr: array [0..15] of int8;');
      add ('   i: 0..15;');
      add ('begin');
      add ('   for i := 0 to 15 do');
      add ('      p (i, arr[i])');
      add ('end.');
      start_test (47);
      test_abs_value (0, 66);
      test_abs_value (1, 101);
      test_abs_value (2, 102);
      test_abs_value (3, 105);
      test_abs_value (4, 105);
      test_abs_value (5, 105);
      test_abs_value (6, 66);
      test_abs_value (7, 107);
      test_abs_value (8, 66);
      test_abs_value (9, 110);
      test_abs_value (10, 110);
      test_abs_value (11, 66);
      test_abs_value (12, 66);
      test_abs_value (13, 66);
      test_abs_value (14, 66);
      test_abs_value (15, 66);
      conclude_test
   end;

procedure test48;    // case - unsigned multi-byte selector
   begin
      add ('procedure p (idx: uint16; var result: int8);');
      add ('   begin');
      add ('      case idx of');
      add ('         1: result := 101;');
      add ('         2: result := 102;');
      add ('         3..5: result := 105;');
      add ('         7: result := 107;');
      add ('         9..10: result := 110;');
      add ('      otherwise');
      add ('         result := 66');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   arr: array [0..15] of int8;');
      add ('   i: 0..15;');
      add ('begin');
      add ('   for i := 0 to 15 do');
      add ('      p (i, arr[i])');
      add ('end.');
      start_test (48);
      test_abs_value (0, 66);
      test_abs_value (1, 101);
      test_abs_value (2, 102);
      test_abs_value (3, 105);
      test_abs_value (4, 105);
      test_abs_value (5, 105);
      test_abs_value (6, 66);
      test_abs_value (7, 107);
      test_abs_value (8, 66);
      test_abs_value (9, 110);
      test_abs_value (10, 110);
      test_abs_value (11, 66);
      test_abs_value (12, 66);
      test_abs_value (13, 66);
      test_abs_value (14, 66);
      test_abs_value (15, 66);
      conclude_test
   end;

procedure test49;    // case - signed single byte selector
   begin
      add ('procedure p (idx: int8; var result: int8);');
      add ('   begin');
      add ('      case idx of');
      add ('         1-5: result := 101;');
      add ('         2-5: result := 102;');
      add ('         3-5..5-5: result := 105;');
      add ('         7-5: result := 107;');
      add ('         9-5..10-5: result := 110;');
      add ('      otherwise');
      add ('         result := 66');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   arr: array [0..15] of int8;');
      add ('   i: 0..15;');
      add ('begin');
      add ('   for i := 0 to 15 do');
      add ('      p (i-5, arr[i])');
      add ('end.');
      start_test (49);
      test_abs_value (0, 66);
      test_abs_value (1, 101);
      test_abs_value (2, 102);
      test_abs_value (3, 105);
      test_abs_value (4, 105);
      test_abs_value (5, 105);
      test_abs_value (6, 66);
      test_abs_value (7, 107);
      test_abs_value (8, 66);
      test_abs_value (9, 110);
      test_abs_value (10, 110);
      test_abs_value (11, 66);
      test_abs_value (12, 66);
      test_abs_value (13, 66);
      test_abs_value (14, 66);
      test_abs_value (15, 66);
      conclude_test
   end;

procedure test50;    // case - signed multi-byte selector
   begin
      add ('const offset = 6;');
      add ('procedure p (idx: int16; var result: int8);');
      add ('   begin');
      add ('      case idx of');
      add ('         1-offset: result := 101;');
      add ('         2-offset: result := 102;');
      add ('         3-offset..5-offset: result := 105;');
      add ('         7-offset: result := 107;');
      add ('         9-offset..10-offset: result := 110;');
      add ('      otherwise');
      add ('         result := 66');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   arr: array [0..15] of int8;');
      add ('   i: 0..15;');
      add ('begin');
      add ('   for i := 0 to 15 do');
      add ('      p (i-offset, arr[i])');
      add ('end.');
      start_test (50);
      test_abs_value (0, 66);
      test_abs_value (1, 101);
      test_abs_value (2, 102);
      test_abs_value (3, 105);
      test_abs_value (4, 105);
      test_abs_value (5, 105);
      test_abs_value (6, 66);
      test_abs_value (7, 107);
      test_abs_value (8, 66);
      test_abs_value (9, 110);
      test_abs_value (10, 110);
      test_abs_value (11, 66);
      test_abs_value (12, 66);
      test_abs_value (13, 66);
      test_abs_value (14, 66);
      test_abs_value (15, 66);
      conclude_test
   end;

procedure test51;    // with
   begin
      add ('var');
      add ('  a: array [1..3] of');
      add ('       record i,j: int16');
      add ('       end;');
      add ('  b: array [1..3] of');
      add ('       record ii,jj: int16;');
      add ('       end;');
      add ('  x: int8;');
      add ('begin');
      add (' for x := 1 to 3 do');
      add ('    with a[x]');
      add ('       do begin');
      add ('             i := x + 100;');
      add ('             j := x + 200;');
      add ('             b[x].ii := i-50;');
      add ('             b[x].jj := j-50');
      add ('          end');
      add ('end.');
      start_test (51);
      test_abs_value (0, 0, 101);
      test_abs_value (2, 0, 201);
      test_abs_value (4, 0, 102);
      test_abs_value (6, 0, 202);
      test_abs_value (8, 0, 103);
      test_abs_value (10, 0, 203);

      test_abs_value (12, 0, 101-50);
      test_abs_value (14, 0, 201-50);
      test_abs_value (16, 0, 102-50);
      test_abs_value (18, 0, 202-50);
      test_abs_value (20, 0, 103-50);
      test_abs_value (22, 0, 203-50);
      conclude_test
   end;

procedure test52;
   begin
      add ('type');
      add ('  tc = class');
      add ('    var x: int8;');
      add ('    public');
      add ('    procedure put (xx: int8);');
      add ('       begin x := xx + 10');
      add ('       end;');
      add ('    function gget: int8;');
      add ('      begin result := x + 20 end;');
      add ('   begin');
      add ('   end;');
      add ('var');
      add ('   r: array [1..3] of int8;');
      add ('   c: array [1..3] of tc;');
      add ('   i: 1..3;');
      add ('begin');
      add ('   for i := 1 to 3 do');
      add ('      begin');
      add ('         init c[i];');
      add ('         c[i].put (i);');
      add ('         r[i] := c[i].gget');
      add ('      end');
      add ('end.');
      start_test (52);
      test_abs_value (0, 31);
      test_abs_value (1, 32);
      test_abs_value (2, 33);
      conclude_test
   end;

procedure test53;
   begin
      add ('type');
      add ('  tc = class');
      add ('    var x: int8;');
      add ('   public');
      add ('    procedure put (xx: int8);');
      add ('       begin x := xx + 10');
      add ('       end;');
      add ('    function gget: int8;');
      add ('      begin result := x + 20 end;');
      add ('   begin');
      add ('   end;');
      add ('var');
      add ('   r: array [1..3] of int8;');
      add ('   c: array [1..3] of tc;');
      add ('   i: 1..3;');
      add ('begin');
      add ('   for i := 1 to 3 do');
      add ('      with c[i] do');
      add ('       begin');
      add ('         init c[i];');
      add ('         put (i);');
      add ('         r[i] := gget');
      add ('      end');
      add ('end.');
      start_test (53);
      test_abs_value (0, 31);
      test_abs_value (1, 32);
      test_abs_value (2, 33);
      conclude_test
   end;

procedure test54;    // get/set properties
   begin
      add ('type');
      add ('  tc = class');
      add ('    var x: int8;');
      add ('    public');
      add ('    property ii: int8;');
      add ('       get :');
      add ('          begin');
      add ('           ii := x + 20');
      add ('          end;');
      add ('       set:');
      add ('         begin');
      add ('            x := ii + 10');
      add ('         end;');
      add ('   begin');
      add ('   end;');
      add ('var');
      add ('   r: array [1..3] of int8;');
      add ('   c: array [1..3] of tc;');
      add ('   i: 1..3;');
      add ('begin');
      add ('   for i := 1 to 3 do');
      add ('       begin');
      add ('         init c[i];');
      add ('         c[i].ii := i;');
      add ('         r[i] := c[i].ii');
      add ('      end');
      add ('end.');
      start_test (54);
      test_abs_value (0, 31);
      test_abs_value (1, 32);
      test_abs_value (2, 33);
      conclude_test
   end;

procedure test55;    // with get/set properties
   begin
      add ('type');
      add ('  tc = class');
      add ('    var x: int8;');
      add ('    public');
      add ('    property ii: int8;');
      add ('       set :');
      add ('          begin');
      add ('            x := ii + 10');
      add ('          end;');
      add ('       get:');
      add ('         begin');
      add ('           ii := x + 20');
      add ('         end;');
      add ('   begin');
      add ('   end;');
      add ('var');
      add ('   r: array [1..3] of int8;');
      add ('   c: array [1..3] of tc;');
      add ('   i: 1..3;');
      add ('begin');
      add ('   for i := 1 to 3 do');
      add ('      with c[i] do');
      add ('       begin');
      add ('         init c[i];');
      add ('         ii := i;');
      add ('         r[i] := ii');
      add ('      end');
      add ('end.');
      start_test (55);
      test_abs_value (0, 31);
      test_abs_value (1, 32);
      test_abs_value (2, 33);
      conclude_test
   end;

procedure test56;    // float ieee & pic
   begin
      add ('var');
      add (' r1,r2,r3,r4: real;');    // 0,4,8,12
      add (' i1,i2,i3,i4: ieee_single;');  // 16,20,24,28
      add (' r: real := 1.23;');
      add (' i: ieee_single := 1.23;');
      add ('const');
      add (' cr: real = 1.23;');
      add (' ci: ieee_single = 1.23;');
      add ('begin');
      add (' r1 := r;');
      add (' i1 := i;');
      add (' r2 := i;');
      add (' i2 := r;');
      add (' r3 := cr;');
      add (' i3 := ci;');
      add (' r4 := ci;');
      add (' i4 := cr;');
      add ('end.');
      start_test (56);
      test_abs_value (0, $7f, $1d, $70, $a4);
      test_abs_value (4, $7f, $1d, $70, $a4);
      test_abs_value (8, $7f, $1d, $70, $a4);
      test_abs_value (12, $7f, $1d, $70, $a4);
      test_abs_value (16, $3f, $9d, $70, $a4);
      test_abs_value (20, $3f, $9d, $70, $a4);
      test_abs_value (24, $3f, $9d, $70, $a4);
      test_abs_value (28, $3f, $9d, $70, $a4);
      conclude_test
   end;

procedure test57;
   begin
      add ('var');
      add (' b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17: boolean;');
      add ('function teq (r1,r2: real): boolean;');
      add ('  begin');
      add ('    result := r1 = r2');
      add ('  end;');
      add ('function tne (r1,r2: real): boolean;');
      add ('  begin');
      add ('    result := r1 <> r2');
      add ('  end;');
      add ('function tlt (r1,r2: real): boolean;');
      add ('  begin');
      add ('    result := r1 < r2');
      add ('  end;');
      add ('function tle (r1,r2: real): boolean;');
      add ('  begin');
      add ('    result := r1 <= r2');
      add ('  end;');
      add ('function tgt (r1,r2: real): boolean;');
      add ('  begin');
      add ('    result := r1 > r2');
      add ('  end;');
      add ('function tge (r1,r2: real): boolean;');
      add ('  begin');
      add ('    result := r1 >= r2');
      add ('  end;');
      add ('begin');
      add (' b0 := teq (1.23, 1.23);');
      add (' b1 := teq (-.8, 1.23);');
      add (' b2 := teq (-78.9, -100.4);');
      add (' b3 := tne (1.23, 1.23);');
      add (' b4 := tne (-.8, 1.23);');
      add (' b5 := tne (-78.9, -100.4); ');
      add (' b6 := tlt (1.23, 1.23);');
      add (' b7 := tlt (-.8, 1.23);');
      add (' b8 := tlt (-78.9, -100.4);');
      add (' b9 := tle (1.23, 1.23);');
      add (' b10 := tle (-.8, 1.23);');
      add (' b11 := tle (-78.9, -100.4);');
      add (' b12 := tgt (1.23, 1.23);');
      add (' b13 := tgt (-.8, 1.23);');
      add (' b14 := tgt (-78.9, -100.4);');
      add (' b15 := tge (1.23, 1.23);');
      add (' b16 := tge (-.8, 1.23);');
      add (' b17 := tge (-78.9, -100.4);');
      add ('end.');
      start_test (57);
      test_abs_value (0, 1);
      test_abs_value (1, 0);
      test_abs_value (2, 0);
      test_abs_value (3, 0);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 0);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 1);
      test_abs_value (10, 1);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 1);
      test_abs_value (15, 1);
      test_abs_value (16, 0);
      test_abs_value (17, 1);
      conclude_test
   end;

procedure test58;
   begin
      add ('var');
      add (' b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17: boolean;');
      add ('function teq (r1,r2: ieee_single): boolean;');
      add ('  begin');
      add ('    result := r1 = r2');
      add ('  end;');
      add ('function tne (r1,r2: ieee_single): boolean;');
      add ('  begin');
      add ('    result := r1 <> r2');
      add ('  end;');
      add ('function tlt (r1,r2: ieee_single): boolean;');
      add ('  begin');
      add ('    result := r1 < r2');
      add ('  end;');
      add ('function tle (r1,r2: ieee_single): boolean;');
      add ('  begin');
      add ('    result := r1 <= r2');
      add ('  end;');
      add ('function tgt (r1,r2: ieee_single): boolean;');
      add ('  begin');
      add ('    result := r1 > r2');
      add ('  end;');
      add ('function tge (r1,r2: ieee_single): boolean;');
      add ('  begin');
      add ('    result := r1 >= r2');
      add ('  end;');
      add ('begin');
      add (' b0 := teq (1.23, 1.23);');
      add (' b1 := teq (-.8, 1.23);');
      add (' b2 := teq (-78.9, -100.4);');
      add (' b3 := tne (1.23, 1.23);');
      add (' b4 := tne (-.8, 1.23);');
      add (' b5 := tne (-78.9, -100.4); ');
      add (' b6 := tlt (1.23, 1.23);');
      add (' b7 := tlt (-.8, 1.23);');
      add (' b8 := tlt (-78.9, -100.4);');
      add (' b9 := tle (1.23, 1.23);');
      add (' b10 := tle (-.8, 1.23);');
      add (' b11 := tle (-78.9, -100.4);');
      add (' b12 := tgt (1.23, 1.23);');
      add (' b13 := tgt (-.8, 1.23);');
      add (' b14 := tgt (-78.9, -100.4);');
      add (' b15 := tge (1.23, 1.23);');
      add (' b16 := tge (-.8, 1.23);');
      add (' b17 := tge (-78.9, -100.4);');
      add ('end.');
      start_test (58);
      test_abs_value (0, 1);
      test_abs_value (1, 0);
      test_abs_value (2, 0);
      test_abs_value (3, 0);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 0);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 1);
      test_abs_value (10, 1);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 1);
      test_abs_value (15, 1);
      test_abs_value (16, 0);
      test_abs_value (17, 1);
      conclude_test
   end;

procedure test59;     // round
   begin
      add ('var i0,i1,i2,i3,i4,i5,i6,i7: int8;');
      add ('  err8,err11,err14,err17: uint24;');
      add ('  x: int24;');
      add ('r: real;');
      add ('begin');
      add (' r := 1.1; i0 := round24(r);');
      add (' r := 1.9; i1 := round24(r);');
      add (' r := -1.1; i2 := round24(r);');
      add (' r := -1.9; i3 := round24(r);');
      add (' r := 1.1; i4 := round32(r);');
      add (' r := 1.9; i5 := round32(r);');
      add (' r := -1.1; i6 := round32(r);');
      add (' r := -1.9; i7 := round32(r);');
      add ('end.');
      start_test (59);
      test_abs_value (0, 1);
      test_abs_value (1, 2);
      test_abs_value (2, $ff);
      test_abs_value (3, $fe);
      test_abs_value (4, 1);
      test_abs_value (5, 2);
      test_abs_value (6, $ff);
      test_abs_value (7, $fe);
      conclude_test
   end;

procedure test60;    // value real parameter passing
   begin
      add ('function pr (r: real): real;');
      add ('   begin result := r end;');
      add ('function pi (i: ieee_single): real;');
      add ('   begin result := i end;');
      add ('var');
      add ('   r0,r4,r8,r12,r16,r20,r24,r28,r32,r36,r40,r44,r48,r52,r56,r60,r64,r68: real;');
      add ('   i24: int24 := 5;');
      add ('   i32: int32 := 5;');
      add ('   i40: int40 := 5;');
      add ('   rrr: real := 5.0;');
      add ('   rir: ieee_single := 5.0;');
      add ('   rri: real := 5;');
      add ('   rii: ieee_single := 5;');
      add ('rom');
      add ('   rrrr: real = 5.0;');
      add ('   rrir: ieee_single = 5.0;');
      add ('   rrri: real = 5;');
      add ('   rrii: ieee_single = 5;');
      add ('begin');
      add ('   r0 := pr (i24);');
      add ('   r4 := pr (i32);');
      add ('   r8 := pr (i40);');
      add ('   r12 := pr (rrr);');
      add ('   r16 := pr (rir);');
      add ('   r20 := pi (i24);');
      add ('   r24 := pi (i32);');
      add ('   r28 := pi (i40);');
      add ('   r32 := pi (rrr);');
      add ('   r36 := pi (rir);');
      add ('   r40 := pr (rri);');
      add ('   r44 := pr (rii);');
      add ('   r48 := pi (rri);');
      add ('   r52 := pi (rii);');
      add ('   r56 := pr (rrri);');
      add ('   r60 := pr (rrii);');
      add ('   r64 := pi (rrri);');
      add ('   r68 := pi (rrii);');
      add ('end.');
      start_test (60);
      test_abs_value (0, $81,$20,0,0);   // 5.0
      test_abs_value (4, $81,$20,0,0);   // 5.0
      test_abs_value (8, $81,$20,0,0);   // 5.0
      test_abs_value (12, $81,$20,0,0);  // 5.0
      test_abs_value (16, $81,$20,0,0);  // 5.0
      test_abs_value (20, $81,$20,0,0);  // 5.0
      test_abs_value (24, $81,$20,0,0);  // 5.0
      test_abs_value (28, $81,$20,0,0);  // 5.0
      test_abs_value (32, $81,$20,0,0);  // 5.0
      test_abs_value (36, $81,$20,0,0);  // 5.0
      test_abs_value (40, $81,$20,0,0);  // 5.0
      test_abs_value (44, $81,$20,0,0);  // 5.0
      test_abs_value (48, $81,$20,0,0);  // 5.0
      test_abs_value (52, $81,$20,0,0);  // 5.0
      test_abs_value (56, $81,$20,0,0);  // 5.0
      test_abs_value (60, $81,$20,0,0);  // 5.0
      test_abs_value (64, $81,$20,0,0);  // 5.0
      test_abs_value (68, $81,$20,0,0);  // 5.0
      conclude_test
   end;

procedure test61;    // basic real operators
   begin
      add ('var');
      add ('   r0,r4,r8,r12,r16,r20,r24,r28,r32,r36,r40,r44,r48,r52,r56,r60,r64,r68: real;');
      add ('   rc1: real := 1;');
      add ('   rc2: real := 2;');
      add ('   rc3: real := 3;');
      add ('   rc5: real := 5;');
      add ('   rc10: real := 10;');
      add ('begin');
      add ('   r0 := rc3 + rc2;');
      add ('   r4 := rc10 - rc5;');
      add ('   r8 := rc1 * rc5;');
      add ('   r12 := rc10 / rc2;');
      add ('end.');
      start_test (61);
      test_abs_value (0, $81,$20,0,0);    // 5.0
      test_abs_value (4, $81,$20,0,0);    // 5.0
      test_abs_value (8, $81,$20,0,0);    // 5.0
      test_abs_value (12, $81,$20,0,0);   // 5.0
      conclude_test
   end;

procedure test62;     // trunc
   begin
      add ('var i0,i1,i2,i3,i4,i5,i6,i7: int8;');
      add ('  err8,err11,err14,err17: uint24;');
      add ('  x: int24;');
      add ('r: real;');
      add ('begin');
      add (' r := 1.1; i0 := trunc24(r);');
      add (' r := 1.9; i1 := trunc24(r);');
      add (' r := -1.1; i2 := trunc24(r);');
      add (' r := -1.9; i3 := trunc24(r);');
      add (' r := 1.1; i4 := trunc32(r);');
      add (' r := 1.9; i5 := trunc32(r);');
      add (' r := -1.1; i6 := trunc32(r);');
      add (' r := -1.9; i7 := trunc32(r);');
      add ('end.');
      start_test (62);
      test_abs_value (0, 1);
      test_abs_value (1, 1);
      test_abs_value (2, $ff);
      test_abs_value (3, $ff);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, $ff);
      test_abs_value (7, $ff);
      conclude_test
   end;

procedure test63;    // floating point runtime errors
   begin
      add ('var err0,err3,err6,err9,err12,err15,err18,err21,err24,err27,err30: uint24;');
      add (' r0: real := 0;');
      add (' rbig: real := 3.4028234e38;');    // largest ieee single, but 1/2 of largest pic real
      add (' rsmall: real := 1.17549435e-38;');
      add (' r: real; x: int40;');
      add ('begin');
      add (' r := r / r0; err0 := ErrorCode;');
      add (' r := $100_0000; x := round24(r); err3 := errorcode;');
      add (' r := -$100_0000-1; x := round24(r); err6 := errorcode;');
      add (' r := $1_0000_0000; x := round32(r); err9 := errorcode;');
      add (' r := -$1_0000_0000-1; x := round32(r); err12 := errorcode;');
      add (' r := rbig * rbig; err15 := errorcode;');
      add (' r := rsmall * rsmall; err18 := errorcode;');
      add (' r := rbig + rbig + rbig; err21 := errorcode;');
      add (' r := rsmall / rbig; err24 := errorcode;');
      add (' r := rbig / rsmall; err27 := errorcode;');
      add ('end.');
      start_test (63);
      test_run_time_error_detected (0, rterr_floating_point_zero_divide);
      test_run_time_error_detected (3, rterr_integer_overflow);
      test_run_time_error_detected (6, rterr_integer_overflow);
      test_run_time_error_detected (9, rterr_integer_overflow);
      test_run_time_error_detected (12, rterr_integer_overflow);
      test_run_time_error_detected (15, rterr_floating_point_overflow);
      test_run_time_error_detected (18, rterr_floating_point_underflow);
      test_run_time_error_detected (21, rterr_floating_point_overflow);
      test_run_time_error_detected (24, rterr_floating_point_underflow);
      test_run_time_error_detected (27, rterr_floating_point_overflow);
      conclude_test
   end;


procedure test64;    // mixed simple expressions
   begin
      add ('var');
      add ('  r0,r4,r8,r12,r16,r20: real;');
      add ('  rc1: real := 1;');
      add ('  rc2: real := 2;');
      add ('  rc3: real := 3;');
      add (' ic1: int8 := 1;');
      add (' ic3: int8 := 3;');
      add (' ic5: int8 := 5;');
      add (' ic7: int8 := 7;');
      add (' ic9: int8 := 9;');
      add (' ic10: int8 := 10;');
      add ('begin');
      add (' r0 := ic1 + rc3 + ic1;');
      add (' r4 := ic3 + rc3 - ic1;');
      add (' r8 := ic7 - rc3 + ic1;');
      add (' r12 := ic9 - rc3 - ic1;');
      add (' r16 := ic5 * rc1;');
      add (' r20 := ic10 / rc2;');
      add ('end.');
      start_test (64);
      test_abs_value (0, $81,$20,0,0);    // 5.0
      test_abs_value (4, $81,$20,0,0);    // 5.0
      test_abs_value (8, $81,$20,0,0);    // 5.0
      test_abs_value (12, $81,$20,0,0);   // 5.0
      test_abs_value (16, $81,$20,0,0);   // 5.0
      test_abs_value (20, $81,$20,0,0);   // 5.0
      conclude_test
   end;

procedure test65;    // abs
   begin
      add ('type');
      add ('   tm8 = -100..-1;');
      add ('   tm16 = -1000..-1;');
      add ('var');
      add ('   r0, r4:real;');
      add ('   i8,i9,i10: int8;');
      add ('   i11,i13,i15: int16;');
      add ('   r17,r21: real;');
      add ('   rc5: real := 5;');
      add ('   rcm5: real := -5;');
      add ('   ic5a: int8 := 5;');
      add ('   icm5a: int8 := -5;');
      add ('   icm5b: tm8 := -5;');
      add ('   ic6a: int16 := 6;');
      add ('   icm6a: int16 := -6;');
      add ('   icm6b: tm16 := -6;');
      add ('   ricm5: ieee_single := -5;');
      add ('rom');
      add ('   rricm5: ieee_single = -5;');
      add ('begin');
      add ('   r0 := abs(rc5);');
      add ('   r4 := abs(rcm5);');
      add ('   i8 := abs(ic5a);');
      add ('   i9 := abs(icm5a);');
      add ('   i10 := abs(icm5b);');
      add ('   i11 := abs(ic6a);');
      add ('   i13 := abs(icm6a);');
      add ('   i15 := abs(icm6b);');
      add ('   r17 := abs(ricm5);');
      add ('   r21 := abs(rricm5);');
      add ('end.');
      start_test (65);
      test_abs_value (0, $81,$20,0,0);    // 5.0
      test_abs_value (4, $81,$20,0,0);    // 5.0
      test_abs_value (8, 5);
      test_abs_value (9, 5);
      test_abs_value (10, 5);
      test_abs_value (11, 0, 6);
      test_abs_value (13, 0, 6);
      test_abs_value (15, 0, 6);
      test_abs_value (17, $81,$20,0,0);    // 5.0
      test_abs_value (21, $81,$20,0,0);    // 5.0
      conclude_test
   end;

procedure test66;    // not, succ, pred & unary minus
   begin
      add ('var');
      add ('  b0,b1: boolean;');
      add ('  i2,i3: int8;');
      add ('  i4,i6: int16;');
      add ('  i8: int16;');
      add ('  r10: real;');
      add ('  _b: boolean;');
      add ('  _i8: int8 := 5;');
      add ('  _i16: int16 := 5;');
      add ('  _im5: int16 := -5;');
      add ('  _rm5: real := -5;');
      add ('begin');
      add (' _b := true; b0 := not _b;');
      add (' _b := false; b1 := not _b;');
      add (' i2 := succ(_i8);');
      add (' i3 := pred(_i8);');
      add (' i4 := succ(_i16);');
      add (' i6 := pred(_i16);');
      add (' i8 := -_im5;');
      add (' r10 := -_rm5;');
      add ('end.');
      start_test (66);
      test_abs_value (0, 0);
      test_abs_value (1, 1);
      test_abs_value (2, 6);
      test_abs_value (3, 4);
      test_abs_value (4, 0, 6);
      test_abs_value (6, 0, 4);
      test_abs_value (8, 0, 5);
      test_abs_value (10, $81,$20,0,0);    // 5.0
      conclude_test
   end;

procedure test67;    // eeprom
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i2,i4,i6: int16;');
      add ('   c1, c2:');
      add ('      class (x: int16);');
      add ('         eeprom');
      add ('            i: int16;');
      add ('      public');
      add ('         function f: int16;');
      add ('            begin');
      add ('               result := i');
      add ('            end;');
      add ('         function f2: int16;');
      add ('            begin');
      add ('               result := i + 3');
      add ('            end;');
      add ('         begin');
      add ('            i := x');
      add ('         end;');
      add ('begin');
      add ('   init c1 (6);');
      add ('   init c2 (7);');
      add ('   i0 := c1.f;');
      add ('   i2 := c2.f;');
      add ('   i4 := c1.f2;');
      add ('   i6 := c2.f2;');
      add ('end.');
      start_test (67);
      test_abs_value (0, 0, 6);
      test_abs_value (2, 0, 7);
      test_abs_value (4, 0, 9);
      test_abs_value (6, 0, 10);
      conclude_test
   end;

procedure test68;    // int to real conversion in assignment, also ieee/pic in assignment
   begin
      add ('const c5=5;');
      add ('var r0,r4,r8,r12: real;');
      add (' i16,i20,i24,i28: ieee_single;');
      add (' r32,r36: real;');
      add (' ar: real := 1.23;');
      add (' ai: ieee_single := 1.23;');
      add (' i: int24; j: int32; k: int40;');
      add ('begin');
      add (' i := 5; r0 := i;');
      add (' j := 5; r4 := j;');
      add (' k := 5; r8 := k;');
      add (' r12:=c5;');
      add (' i16 := ai;');   // single := single
      add (' i20 := ar;');   // single := pic
      add (' r32 := ai;');   // pic := single
      add (' r36 := ar;');   // pic := pic
      add ('end.');
      start_test (68);
      test_abs_value (0, $81,$20,0,0);    // 5.0
      test_abs_value (4, $81,$20,0,0);    // 5.0
      test_abs_value (8, $81,$20,0,0);    // 5.0
      test_abs_value (12, $81,$20,0,0);    // 5.0
      test_abs_value (16, $3f, $9d, $70, $a4);
      test_abs_value (20, $3f, $9d, $70, $a4);
      test_abs_value (32, $7f, $1d, $70, $a4);
      test_abs_value (36, $7f, $1d, $70, $a4);
      conclude_test
   end;

procedure test69;    // string assignment
   begin
      add ('{$processor ''pic18f2520''}');
      add ('rom');
      add ('   rs: string[5] = ''abc'';');
      add ('   r0: string[5] = '''';');
      add ('type');
      add ('   ct=class');
      add ('      eeprom cs: string[3] := ''abc'';');
      add ('         ex1: string[1];');
      add ('         sz,sz2: string[2];');
      add ('   var s: string[3];');
      add ('      s2: string[2];');
      add ('      szz: string [1];');
      add ('   procedure pex (eeprom s: string);');
      add ('      var c: char := ''e'';');
      add ('      begin s := c end;');
      add ('   public');
      add ('   procedure x (var sp: string);');
      add ('      begin');
      add ('         sp := s');
      add ('      end;');
      add ('   property ps: string;');
      add ('      get: begin ps := s end;');
      add ('   function getex1: string;');
      add ('      begin');
      add ('         pex (ex1);');
      add ('         result := ex1');
      add ('      end;');
      add ('   function fez: uint8;');
      add ('      begin');
      add ('        sz.strlen := 1;');
      add ('        sz := szz;');
      add ('        result := sz.strlen');
      add ('      end;');
      add ('   procedure teste0 (var iera, iee, iero: uint8);');
      add ('      begin');
      add ('        sz.strlen := 1; szz.strlen := 0; sz := szz; iera := sz.strlen;');
      add ('        sz.strlen := 1; sz := sz2; iee := sz.strlen;');
      add ('        sz.strlen := 1; sz := r0; iero := sz.strlen;');
      add ('      end;');
      add ('   begin');
      add ('      s := cs;             // RESULT NOT TESTED YET!!!');
      add ('      s2 := cs;');
      add ('   end;');
      add ('procedure p (var s1, s2: string);');
      add ('   begin');
      add ('      s1 := ''abc'';');
      add ('      s2 := ''abc'';');
      add ('   end;');
      add ('function f: string;');
      add ('   begin');
      add ('      result := ''abc'';');
      add ('   end;');
      add ('procedure xx (var s: string);');
      add ('   var c: char := ''a'';');
      add ('   begin s := c end;');
      add ('var');
      add ('   s: string[3];     // 0');
      add ('   err: uint24;      // 4');
      add ('   s3: string[2];    // 7');
      add ('   s0: string[3];    // 10');
      add ('   s03: string[2];    // 14');
      add ('   err2: uint24;     // 17');
      add ('   s9: string[5];     // 20');
      add ('   err3: uint24;     // 26');
      add ('   sp2: string[3];     // 29');
      add ('   err4: uint24;      // 33');
      add ('   sc: string[3];      // 36');
      add ('   sf: string[3];       // 40');
      add ('   err5: uint24;     // 44');
      add ('   scp: string[3];     //47');
      add ('   xxx: char;        // 51');
      add ('   sx2: string[1];     // 52');
      add ('   err54: uint24;     // 54');
      add ('   scpex: string[2];     // 57');
      add ('   err6: uint24;     // 60');
      add ('   szz: string[4];      // 63');
      add ('   ezr: uint8;     // 68');
      add ('   sr0: string[1];     // 69');
      add ('   iera, iee, iero: uint8;     // 71,72,73');
      add ('   s04: string[2];');
      add ('   s1: string[2];');
      add ('   s2: string[5] := ''abc'';');
      add ('   sz: string[5];');
      add ('   c: ct;');
      add ('begin');
      add ('   s := s2;');
      add ('   s3 := s1;');
      add ('   s1 := s2;');
      add ('   err := ErrorCode;');
      add ('   s0 := rs;');
      add ('   s03 := r0;');
      add ('   s04 := rs;');
      add ('   err2 := ErrorCode;');
      add ('   s9 := ''abc'';');
      add ('   init c;');
      add ('   err3 := ErrorCode;');
      add ('   p (sp2, s04);');
      add ('   err4 := ErrorCode;');
      add ('   c.x(sc);');
      add ('   sf := f;');
      add ('   s04 := f;');
      add ('   err5 := ErrorCode;');
      add ('   scp := c.ps;');
      add ('   xx (sx2);');
      add ('   scpex := c.getex1;');
      add ('   szz.strlen := 3;');
      add ('   szz := sz;');
      add ('   ezr := c.fez;');
      add ('   sr0.strlen := 1; sr0 := r0;');
      add ('   c.teste0 (iera, iee, iero);');
      add ('end.');
      start_test (69);
      test_abs_value (0, 3);
      test_abs_value (1, $61);
      test_abs_value (2, $62);
      test_abs_value (3, $63);
      test_run_time_error_detected (4, rterr_string_overflow);
      test_abs_value (7, 0);
      test_abs_value (10, 3);
      test_abs_value (11, $61);
      test_abs_value (12, $62);
      test_abs_value (13, $63);
      test_abs_value (14, 0);
      test_run_time_error_detected (17, rterr_string_overflow);
      test_abs_value (20, 3);
      test_abs_value (21, $61);
      test_abs_value (22, $62);
      test_abs_value (23, $63);
      test_abs_value (24, 0);
      test_run_time_error_detected (26, rterr_string_overflow);
      test_abs_value (29, 3);
      test_abs_value (30, $61);
      test_abs_value (31, $62);
      test_abs_value (32, $63);
      test_run_time_error_detected (33, rterr_string_overflow);
      test_abs_value (36, 3);
      test_abs_value (37, $61);
      test_abs_value (38, $62);
      test_abs_value (39, $63);
      test_abs_value (40, 3);
      test_abs_value (41, $61);
      test_abs_value (42, $62);
      test_abs_value (43, $63);
      test_run_time_error_detected (44, rterr_string_overflow);
      test_abs_value (47, 3);
      test_abs_value (48, $61);
      test_abs_value (49, $62);
      test_abs_value (50, $63);
      test_abs_value (52, 1, ord('a'));
      test_abs_value (57, 1, ord('e'));
      test_abs_value (63, 0);
      test_abs_value (68, 0);
      test_abs_value (69, 0);
      test_abs_value (71, 0);
      test_abs_value (72, 0);
      test_abs_value (73, 0);
      conclude_test
   end;

procedure test70;    // eeprom string assignment
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('   ct=class');
      add ('         rom');
      add ('            r_init: string [3] = ''abc'';');
      add ('         var');
      add ('            s_init: string [3] := ''abc'';');
      add ('         eeprom');
      add ('            s: string [3];');
      add ('            e: string [2];');
      add ('            sr: string [3];');
      add ('            se: string [3];');
      add ('        public');
      add ('         function fs: string;');
      add ('            begin');
      add ('               result := s');
      add ('            end;');
      add ('         procedure test_rom;');
      add ('            begin');
      add ('               sr := r_init;');
      add ('            end;');
      add ('         function fs2: string;');
      add ('            begin');
      add ('               result := sr;');
      add ('               e := r_init;');
      add ('            end;');
      add ('         procedure test_eeprom;');
      add ('            begin');
      add ('               se := s;');
      add ('            end;');
      add ('         function fs3: string;');
      add ('            begin');
      add ('               result := se;');
      add ('               e := se;');
      add ('            end;');
      add ('         begin');
      add ('            s := s_init;');
      add ('            e := s_init;');
      add ('         end;');
      add ('var');
      add ('   s1: string [3];'); // 0
      add ('   err1,err2: uint24;');   // 4,7
      add ('   s2,s3: string [3];'); // 10,14
      add ('   err3: uint24;');  // 18
      add ('   c: ct;');
      add ('begin');
      add ('   init c;');
      add ('   err1 := ErrorCode;');
      add ('   s1 := c.fs;');
      add ('   c.test_rom;');
      add ('   s2 := c.fs2;');
      add ('   err2 := ErrorCode;');
      add ('   c.test_eeprom;');
      add ('   s3 := c.fs3;');
      add ('   err3 := ErrorCode;');
      add ('end.');
      start_test (70);
      test_abs_value (0, 3);
      test_abs_value (1, $61);
      test_abs_value (2, $62);
      test_abs_value (3, $63);
      test_run_time_error_detected (4, rterr_string_overflow);
      test_abs_value (10, 3);
      test_abs_value (11, $61);
      test_abs_value (12, $62);
      test_abs_value (13, $63);
      test_run_time_error_detected (7, rterr_string_overflow);
      test_abs_value (14, 3);
      test_abs_value (15, $61);
      test_abs_value (16, $62);
      test_abs_value (17, $63);
      test_run_time_error_detected (18, rterr_string_overflow);
      conclude_test
   end;

procedure test71;    // strlen assignment
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('  tc=class');
      add ('    eeprom s: string[5] := ''abc'';');
      add ('   public');
      add ('    function slen: uint8;');
      add ('      begin result := s.strlen end;');
      add ('    function smax: uint8;');
      add ('      begin result := s.maxstrlen end;');
      add ('    procedure setlen (i: uint8);');
      add ('      begin s.strlen := i end;');
      add ('    begin');
      add ('    end;');
      add ('rom');
      add ('   rs: string[5] = ''abc'';');
      add ('procedure p (var s: string; i: int16);');
      add ('   begin');
      add ('     s.strlen := i');
      add ('   end;');
      add ('var');
      add ('   s: string[3];  // 0');
      add ('   j,k,l: int8;  // 4,5,6');
      add ('   err1: uint24;  // 7');
      add ('   m, n: int8;   // 10,11');
      add ('   err2: uint24; // 12');
      add ('   o,x: uint8;    // 15,16');
      add ('   err3, err4: uint24;  // 17, 20');
      add ('   i23,i24,i25,i26,i27,i28,i29: uint8;');
      add ('   err30: uint24;');
      add ('   i: int8;');
      add ('   s255: string[255];');
      add ('   c: tc;');
      add ('begin');
      add ('   i := 0;');
      add ('   s.strlen := i;');
      add ('   j := s.strlen;');
      add ('   i := 3;');
      add ('   s.strlen := i;');
      add ('   k := s.strlen;');
      add ('   i := 4;');
      add ('   s.strlen := i;');
      add ('   l := s.strlen;');
      add ('   err1 := ErrorCode;');
      add ('   p (s, 0);');
      add ('   m := s.strlen;');
      add ('   p (s, 3);');
      add ('   n := s.strlen;');
      add ('   p (s, 4);');
      add ('   err2 := ErrorCode;');
      add ('   p (s255, 0);');
      add ('   o := s255.strlen;');
      add ('   p (s255, 255);');
      add ('   x := s255.strlen;');
      add ('   p (s255, 256);');
      add ('   err3 := ErrorCode;');
      add ('   p (s255, -1);');
      add ('   err4 := ErrorCode;');
      add ('   i23 := rs.strlen;');
      add ('   i24 := rs.maxstrlen;');
      add ('   i25 := c.slen;');
      add ('   i26 := c.smax;');
      add ('   c.setlen (2);');
      add ('   i27 := c.slen;');
      add ('   c.setlen (5);');
      add ('   i28 := c.slen;');
      add ('   c.setlen (6);');
      add ('   err30 := ErrorCode;');
      add ('   i29 := c.slen;');
      add ('end.');
      start_test (71);
      test_abs_value (4, 0);
      test_abs_value (5, 3);
      test_abs_value (6, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 3);
      test_abs_value (15, 0);
      test_abs_value (16, 255);
      test_run_time_error_detected (7, rterr_invalid_strlen);
      test_run_time_error_detected (12, rterr_invalid_strlen);
      test_run_time_error_detected (17, rterr_invalid_strlen);
      test_run_time_error_detected (20, rterr_invalid_strlen);
      test_abs_value (23, 3);
      test_abs_value (24, 5);
      test_abs_value (25, 3);
      test_abs_value (26, 5);
      test_abs_value (27, 2);
      test_abs_value (28, 5);
      test_run_time_error_detected (30, rterr_invalid_strlen);
      test_abs_value (29, 0);
      conclude_test
   end;

procedure test72;    // maxstrlen
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('tc=class');
      add ('     eeprom');
      add ('       s: string[5] := ''abc'';');
      add ('     function x (eeprom ss: string): uint8;');
      add ('        begin');
      add ('           result := ss.maxstrlen');
      add ('        end;');
      add ('     public');
      add ('     function fm: uint8;');
      add ('        begin');
      add ('          result := x(s)');
      add ('        end;');
      add ('     function fd: uint8;');
      add ('        rom c:int8=5;');
      add ('        begin');
      add ('           result := s.maxstrlen');
      add ('        end;');
      add ('     begin');
      add ('     end;');
      add ('rom');
      add ('   rs: string[5] = ''abc'';');
      add ('function fv (var s: string): uint8;');
      add ('  begin');
      add ('     result := s.maxstrlen');
      add ('  end;');
      add ('function fc (s: string): uint8;');
      add ('  begin');
      add ('    result := s.maxstrlen');
      add ('  end;');
      add ('function fr (rom s: string): uint8;');
      add ('  begin');
      add ('    result := s.maxstrlen');
      add ('  end;');
      add ('var');
      add ('  i0,i1,i2,i3,i4,i5,i6: uint8;');
      add ('  s: string[5] := ''abc'';');
      add ('  c: tc;');
      add ('begin');
      add ('  i0 := fv(s);');
      add ('  i1 := fc(s);');
      add ('  i2 := s.maxstrlen;');
      add ('  i3 := rs.maxstrlen;');
      add ('  i4 := fr (rs);');
      add ('  i5 := c.fm;');
      add ('  i6 := c.fd;');
      add ('end.');
      start_test (72);
      test_abs_value (0, 5);
      test_abs_value (1, 3);
      test_abs_value (2, 5);
      test_abs_value (3, 5);
      test_abs_value (4, 3);
      test_abs_value (5, 5);
      test_abs_value (5, 5);
      conclude_test
   end;

procedure test73;  // strappend
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add (' tc = class');
      add ('   eeprom s: string[2] := ''a'';');
      add ('     es: string[5] := ''cde'';');
      add ('     ec: string[4] := ''mn'';');
      add ('     esr: string[5];');
      add ('     esram: string[5];');
      add ('      ese: string[5] := ''x'';');
      add ('   var ss: string [5] := ''ab'';');
      add ('   rom');
      add ('      rs: string[5] = ''aBc'';');
      add ('   public');
      add ('   function read_ese: string;');
      add ('     begin');
      add ('       result := ese');
      add ('     end;');
      add ('   function read_esram: string;');
      add ('     begin');
      add ('       result := esram');
      add ('     end;');
      add ('   procedure p;');
      add ('      begin');
      add ('         s.strappend (''x'')');
      add ('      end;');
      add ('   function f: string;');
      add ('      var t: string[2];');
      add ('      begin');
      add ('         t := s;');
      add ('         result := t');
      add ('      end;');
      add ('   function ff: string;');
      add ('     begin');
      add ('        result := ss');
      add ('     end;');
      add ('   procedure pp;');
      add ('     begin');
      add ('       ss.strappend(es)');
      add ('     end;');
      add ('   function read_esr: string;');
      add ('     begin');
      add ('       esr.strappend (rs);');
      add ('       result := esr');
      add ('     end;');
      add ('   function read_ec: string;');
      add ('     begin');
      add ('       result := ec');
      add ('     end;');
      add ('   begin');
      add ('     ese.strappend (es);');
      add ('     esram.strappend (ss);');
      add ('     s.strappend(''b'');');
      add ('     ss.strappend (es);');
      add ('     ec.strappend (''op'');');
      add ('   end;');
      add ('var');
      add ('   err0: uint24;             // 0');
      add ('   s: string [2] := ''a'';     // 3');
      add ('   err6: uint24;');
      add ('   s9: string[2];');
      add ('   s12: string [5] := ''a'';');
      add ('   s18: string [5] := ''xy'';');
      add ('   s24: string [6];');
      add ('   err31: uint24;');
      add ('   s34: string[5];');
      add ('   s40: string[4];');
      add ('   s45: string[4];');
      add ('   s50: string[4];');
      add ('   err55, err58, err61: uint24;');
      add ('   c: tc;');
      add ('rom');
      add ('   rs: string[5] = ''de'';');
      add ('begin');
      add ('      //       test append char to str');
      add ('   s.strappend (''b'');');
      add ('   s.strappend (''c'');');
      add ('   err0 := ErrorCode;');
      add ('      //       test append char to eeprom');
      add ('   init c;');
      add ('   s9 := c.f;');
      add ('   c.p;');
      add ('   err6 := ErrorCode;');
      add ('      //  test append ram str to ram str');
      add ('   s12.strappend (s18);');
      add ('      //  test append rom str to ram str');
      add ('   s18.strappend (rs);');
      add ('   s24 := c.ff;');
      add ('   c.pp;');
      add ('    err31 := ErrorCode;');
      add ('    s34 := c.read_ec;');
      add ('    s40 := c.read_esr;');
      add ('    s45 := c.read_esram;');
      add ('    s50 := c.read_ese;');
      add ('end.');
      start_test (73);
      test_abs_value (3, 2, ord('a'), ord('b'));
      test_run_time_error_detected (0, rterr_string_overflow);
      test_abs_value (9, 2, ord('a'), ord('b'));
      test_run_time_error_detected (6, rterr_string_overflow);
      test_abs_value (12, 3, ord('a'), ord('x'), ord('y'));
      test_abs_value (18, 4, ord('x'), ord('y'), ord('d'), ord('e')); test_abs_value (23, 0);
      test_abs_value (24, 5, ord ('a'), ord('b')); test_abs_value (27, ord('c'), ord('d'), ord('e'), 0);
      test_run_time_error_detected (31, rterr_string_overflow);
      test_abs_value (34, 4, ord ('m'), ord('n'), ord('o'), ord ('p'));
      test_abs_value (40, 3, ord('a'), ord('B'), ord('c'));
      test_abs_value (45, 2, ord('a'), ord('b'));
      test_abs_value (50, 4, ord('x'), ord('c'), ord('d'), ord('e'));
      conclude_test
   end;

procedure test74;
   begin
      MainForm.TestResultsMemo.Lines.Add ('test 74');
      TSubroutine.check_stack_calculations;
      MainForm.number_of_tests := MainForm.number_of_tests + 1
   end;

procedure test75;  // compare ram str to ram str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('   s1: string [5] := ''abc'';');
      add ('   s2: string [5] := ''ab'';');
      add ('   s3: string [5] := ''ax'';');
      add ('   s4: string [5];');
      add ('begin');
      add ('   b0 := s1 = s1;');
      add ('   b1 := s1 <> s1;');
      add ('   b2 := s1 < s1;');
      add ('   b3 := s1 > s1;');
      add ('   b4 := s1 <= s1;');
      add ('   b5 := s1 >= s1;');
      add ('   b6 := s1 = s2;');
      add ('   b7 := s1 <> s2;');
      add ('   b8 := s1 < s2;');
      add ('   b9 := s1 > s2;');
      add ('   b10 := s1 <= s2;');
      add ('   b11 := s1 >= s2;');
      add ('   b12 := s2 = s1;');
      add ('   b13 := s2 <> s1;');
      add ('   b14 := s2 < s1;');
      add ('   b15 := s2 > s1;');
      add ('   b16 := s2 <= s1;');
      add ('   b17 := s2 >= s1;');
      add ('   b18 := s1 = s3;');
      add ('   b19 := s1 <> s3;');
      add ('   b20 := s1 < s3;');
      add ('   b21 := s1 > s3;');
      add ('   b22 := s1 <= s3;');
      add ('   b23 := s1 >= s3;');
      add ('   b24 := s3 = s2;');
      add ('   b25 := s3 <> s2;');
      add ('   b26 := s3 < s2;');
      add ('   b27 := s3 > s2;');
      add ('   b28 := s3 <= s2;');
      add ('   b29 := s3 >= s2;');
      add ('   b30 := s1 = s4;');
      add ('   b31 := s1 <> s4;');
      add ('   b32 := s1 < s4;');
      add ('   b33 := s1 > s4;');
      add ('   b34 := s1 <= s4;');
      add ('   b35 := s1 >= s4;');
      add ('   b36 := s4 = s2;');
      add ('   b37 := s4 <> s2;');
      add ('   b38 := s4 < s2;');
      add ('   b39 := s4 > s2;');
      add ('   b40 := s4 <= s2;');
      add ('   b41 := s4 >= s2;');
      add ('end.');
      start_test (75);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      add ('');
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      add ('');
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test76;  // compare ram str to rom str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('   s1: string [5] := ''abc'';');
      add ('   s2: string [5] := ''ab'';');
      add ('   s3: string [5] := ''ax'';');
      add ('   s4: string [5];');
      add ('rom');
      add ('   r1: string [5] = ''abc'';');
      add ('   r2: string [5] = ''ab'';');
      add ('   r3: string [5] = ''ax'';');
      add ('   r4: string [5] = '''';');
      add ('begin');
      add ('   b0 := s1 = r1;');
      add ('   b1 := s1 <> r1;');
      add ('   b2 := s1 < r1;');
      add ('   b3 := s1 > r1;');
      add ('   b4 := s1 <= r1;');
      add ('   b5 := s1 >= r1;');
      add ('   b6 := s1 = r2;');
      add ('   b7 := s1 <> r2;');
      add ('   b8 := s1 < r2;');
      add ('   b9 := s1 > r2;');
      add ('   b10 := s1 <= r2;');
      add ('   b11 := s1 >= r2;');
      add ('   b12 := s2 = r1;');
      add ('   b13 := s2 <> r1;');
      add ('   b14 := s2 < r1;');
      add ('   b15 := s2 > r1;');
      add ('   b16 := s2 <= r1;');
      add ('   b17 := s2 >= r1;');
      add ('   b18 := s1 = r3;');
      add ('   b19 := s1 <> r3;');
      add ('   b20 := s1 < r3;');
      add ('   b21 := s1 > r3;');
      add ('   b22 := s1 <= r3;');
      add ('   b23 := s1 >= r3;');
      add ('   b24 := s3 = r2;');
      add ('   b25 := s3 <> r2;');
      add ('   b26 := s3 < r2;');
      add ('   b27 := s3 > r2;');
      add ('   b28 := s3 <= r2;');
      add ('   b29 := s3 >= r2;');
      add ('   b30 := s1 = r4;');
      add ('   b31 := s1 <> r4;');
      add ('   b32 := s1 < r4;');
      add ('   b33 := s1 > r4;');
      add ('   b34 := s1 <= r4;');
      add ('   b35 := s1 >= r4;');
      add ('   b36 := s4 = r2;');
      add ('   b37 := s4 <> r2;');
      add ('   b38 := s4 < r2;');
      add ('   b39 := s4 > r2;');
      add ('   b40 := s4 <= r2;');
      add ('   b41 := s4 >= r2;');
      add ('end.');
      start_test (76);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test77;  // compare rom str to ram str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('   s1: string [5] := ''abc'';');
      add ('   s2: string [5] := ''ab'';');
      add ('   s3: string [5] := ''ax'';');
      add ('   s4: string [5];');
      add ('rom');
      add ('   r1: string [5] = ''abc'';');
      add ('   r2: string [5] = ''ab'';');
      add ('   r3: string [5] = ''ax'';');
      add ('   r4: string [5] = '''';');
      add ('begin');
      add ('   b0 := r1 = s1;');
      add ('   b1 := r1 <> s1;');
      add ('   b2 := r1 < s1;');
      add ('   b3 := r1 > s1;');
      add ('   b4 := r1 <= s1;');
      add ('   b5 := r1 >= s1;');
      add ('   b6 := r1 = s2;');
      add ('   b7 := r1 <> s2;');
      add ('   b8 := r1 < s2;');
      add ('   b9 := r1 > s2;');
      add ('   b10 := r1 <= s2;');
      add ('   b11 := r1 >= s2;');
      add ('   b12 := r2 = s1;');
      add ('   b13 := r2 <> s1;');
      add ('   b14 := r2 < s1;');
      add ('   b15 := r2 > s1;');
      add ('   b16 := r2 <= s1;');
      add ('   b17 := r2 >= s1;');
      add ('   b18 := r1 = s3;');
      add ('   b19 := r1 <> s3;');
      add ('   b20 := r1 < s3;');
      add ('   b21 := r1 > s3;');
      add ('   b22 := r1 <= s3;');
      add ('   b23 := r1 >= s3;');
      add ('   b24 := r3 = s2;');
      add ('   b25 := r3 <> s2;');
      add ('   b26 := r3 < s2;');
      add ('   b27 := r3 > s2;');
      add ('   b28 := r3 <= s2;');
      add ('   b29 := r3 >= s2;');
      add ('   b30 := r1 = s4;');
      add ('   b31 := r1 <> s4;');
      add ('   b32 := r1 < s4;');
      add ('   b33 := r1 > s4;');
      add ('   b34 := r1 <= s4;');
      add ('   b35 := r1 >= s4;');
      add ('   b36 := r4 = s2;');
      add ('   b37 := r4 <> s2;');
      add ('   b38 := r4 < s2;');
      add ('   b39 := r4 > s2;');
      add ('   b40 := r4 <= s2;');
      add ('   b41 := r4 >= s2;');
      add ('end.');
      start_test (77);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test78;  // compare rom str to rom str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('rom');
      add ('   r1: string [5] = ''abc'';');
      add ('   r2: string [5] = ''ab'';');
      add ('   r3: string [5] = ''ax'';');
      add ('   r4: string [5] = '''';');
      add ('begin');
      add ('   b0 := r1 = r1;');
      add ('   b1 := r1 <> r1;');
      add ('   b2 := r1 < r1;');
      add ('   b3 := r1 > r1;');
      add ('   b4 := r1 <= r1;');
      add ('   b5 := r1 >= r1;');
      add ('   b6 := r1 = r2;');
      add ('   b7 := r1 <> r2;');
      add ('   b8 := r1 < r2;');
      add ('   b9 := r1 > r2;');
      add ('   b10 := r1 <= r2;');
      add ('   b11 := r1 >= r2;');
      add ('   b12 := r2 = r1;');
      add ('   b13 := r2 <> r1;');
      add ('   b14 := r2 < r1;');
      add ('   b15 := r2 > r1;');
      add ('   b16 := r2 <= r1;');
      add ('   b17 := r2 >= r1;');
      add ('   b18 := r1 = r3;');
      add ('   b19 := r1 <> r3;');
      add ('   b20 := r1 < r3;');
      add ('   b21 := r1 > r3;');
      add ('   b22 := r1 <= r3;');
      add ('   b23 := r1 >= r3;');
      add ('   b24 := r3 = r2;');
      add ('   b25 := r3 <> r2;');
      add ('   b26 := r3 < r2;');
      add ('   b27 := r3 > r2;');
      add ('   b28 := r3 <= r2;');
      add ('   b29 := r3 >= r2;');
      add ('   b30 := r1 = r4;');
      add ('   b31 := r1 <> r4;');
      add ('   b32 := r1 < r4;');
      add ('   b33 := r1 > r4;');
      add ('   b34 := r1 <= r4;');
      add ('   b35 := r1 >= r4;');
      add ('   b36 := r4 = r2;');
      add ('   b37 := r4 <> r2;');
      add ('   b38 := r4 < r2;');
      add ('   b39 := r4 > r2;');
      add ('   b40 := r4 <= r2;');
      add ('   b41 := r4 >= r2;');
      add ('end.');
      start_test (78);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test79;  // compare ram str to eeprom str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('   tc=class');
      add ('         eeprom e: string[5];');
      add ('         var s: string[5];');
      add ('     public');
      add ('      procedure equal (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e = s');
      add ('         end;');
      add ('      procedure not_equal (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <> s');
      add ('         end;');
      add ('      procedure less (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e < s');
      add ('         end;');
      add ('      procedure greater (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e > s');
      add ('         end;');
      add ('      procedure lessorequal (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <= s');
      add ('         end;');
      add ('      procedure greaterorequal(var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e >= s');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('   s1: string [5] := ''abc'';');
      add ('   s2: string [5] := ''ab'';');
      add ('   s3: string [5] := ''ax'';');
      add ('   s4: string [5];');
      add ('   c: tc;');
      add ('begin');
      add ('   c.equal (b0, s1, s1);');
      add ('   c.not_equal (b1, s1, s1);');
      add ('   c.less (b2, s1, s1);');
      add ('   c.greater (b3, s1, s1);');
      add ('   c.lessorequal (b4, s1, s1);');
      add ('   c.greaterorequal (b5, s1, s1);');
      add ('   c.equal (b6, s1, s2);');
      add ('   c.not_equal (b7, s1, s2);');
      add ('   c.less (b8, s1, s2);');
      add ('   c.greater (b9, s1, s2);');
      add ('   c.lessorequal (b10, s1, s2);');
      add ('   c.greaterorequal (b11, s1, s2);');
      add ('   c.equal (b12, s2, s1);');
      add ('   c.not_equal (b13, s2, s1);');
      add ('   c.less (b14, s2, s1);');
      add ('   c.greater (b15, s2, s1);');
      add ('   c.lessorequal (b16, s2, s1);');
      add ('   c.greaterorequal (b17, s2, s1);');
      add ('   c.equal (b18, s1, s3);');
      add ('   c.not_equal (b19, s1, s3);');
      add ('   c.less (b20, s1, s3);');
      add ('   c.greater (b21, s1, s3);');
      add ('   c.lessorequal (b22, s1, s3);');
      add ('   c.greaterorequal (b23, s1, s3);');
      add ('   c.equal (b24, s3, s2);');
      add ('   c.not_equal (b25, s3, s2);');
      add ('   c.less (b26, s3, s2);');
      add ('   c.greater (b27, s3, s2);');
      add ('   c.lessorequal (b28, s3, s2);');
      add ('   c.greaterorequal (b29, s3, s2);');
      add ('   c.equal (b30, s1, s4);');
      add ('   c.not_equal (b31, s1, s4);');
      add ('   c.less (b32, s1, s4);');
      add ('   c.greater (b33, s1, s4);');
      add ('   c.lessorequal (b34, s1, s4);');
      add ('   c.greaterorequal (b35, s1, s4);');
      add ('   c.equal (b36, s4, s2);');
      add ('   c.not_equal (b37, s4, s2);');
      add ('   c.less (b38, s4, s2);');
      add ('   c.greater (b39, s4, s2);');
      add ('   c.lessorequal (b40, s4, s2);');
      add ('   c.greaterorequal (b41, s4, s2);');
      add ('end.');
      start_test (79);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test80;  // compare eeprom str to ram str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('   tc=class');
      add ('         eeprom e: string[5];');
      add ('         var s: string[5];');
      add ('     public');
      add ('      procedure equal (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e = s');
      add ('         end;');
      add ('      procedure not_equal (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');           // NOTE NAMES HAVE WRONG SENSE IN TEST80
      add ('            b := e <> s');
      add ('         end;');
      add ('      procedure less (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e < s');
      add ('         end;');
      add ('      procedure greater (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e > s');
      add ('         end;');
      add ('      procedure lessorequal (var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <= s');
      add ('         end;');
      add ('      procedure greaterorequal(var b: boolean; s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e >= s');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('   s1: string [5] := ''abc'';');
      add ('   s2: string [5] := ''ab'';');
      add ('   s3: string [5] := ''ax'';');
      add ('   s4: string [5];');
      add ('   c: tc;');
      add ('begin');
      add ('   c.equal (b0, s1, s1);');
      add ('   c.not_equal (b1, s1, s1);');
      add ('   c.less (b2, s1, s1);');
      add ('   c.greater (b3, s1, s1);');
      add ('   c.lessorequal (b4, s1, s1);');
      add ('   c.greaterorequal (b5, s1, s1);');
      add ('   c.equal (b6, s1, s2);');
      add ('   c.not_equal (b7, s1, s2);');
      add ('   c.less (b8, s1, s2);');
      add ('   c.greater (b9, s1, s2);');
      add ('   c.lessorequal (b10, s1, s2);');
      add ('   c.greaterorequal (b11, s1, s2);');
      add ('   c.equal (b12, s2, s1);');
      add ('   c.not_equal (b13, s2, s1);');
      add ('   c.less (b14, s2, s1);');
      add ('   c.greater (b15, s2, s1);');
      add ('   c.lessorequal (b16, s2, s1);');
      add ('   c.greaterorequal (b17, s2, s1);');
      add ('   c.equal (b18, s1, s3);');
      add ('   c.not_equal (b19, s1, s3);');
      add ('   c.less (b20, s1, s3);');
      add ('   c.greater (b21, s1, s3);');
      add ('   c.lessorequal (b22, s1, s3);');
      add ('   c.greaterorequal (b23, s1, s3);');
      add ('   c.equal (b24, s3, s2);');
      add ('   c.not_equal (b25, s3, s2);');
      add ('   c.less (b26, s3, s2);');
      add ('   c.greater (b27, s3, s2);');
      add ('   c.lessorequal (b28, s3, s2);');
      add ('   c.greaterorequal (b29, s3, s2);');
      add ('   c.equal (b30, s1, s4);');
      add ('   c.not_equal (b31, s1, s4);');
      add ('   c.less (b32, s1, s4);');
      add ('   c.greater (b33, s1, s4);');
      add ('   c.lessorequal (b34, s1, s4);');
      add ('   c.greaterorequal (b35, s1, s4);');
      add ('   c.equal (b36, s4, s2);');
      add ('   c.not_equal (b37, s4, s2);');
      add ('   c.less (b38, s4, s2);');
      add ('   c.greater (b39, s4, s2);');
      add ('   c.lessorequal (b40, s4, s2);');
      add ('   c.greaterorequal (b41, s4, s2);');
      add ('end.');
      start_test (80);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test81;  // compare ram str to char
   begin
      add ('type');
      add ('   ba = array [0..5] of boolean;');
      add ('procedure test (var b: ba; s: string; c: char);');
      add ('   begin');
      add ('      b[0] := s = c;');
      add ('      b[1] := s <> c;');
      add ('      b[2] := s < c;');
      add ('      b[3] := s > c;');
      add ('      b[4] := s <= c;');
      add ('      b[5] := s >= c');
      add ('   end;');
      add ('var');
      add ('   b0, b6, b12, b18, b24, b30, b36: ba;');
      add ('   s: string[2];');
      add ('begin');
      add ('   s := ''''; test (b0, s, ''m'');');
      add ('   s := ''a''; test (b6, s, ''m'');');
      add ('   s := ''m''; test (b12, s, ''m'');');
      add ('   s := ''z''; test (b18, s, ''m'');');
      add ('   s := ''mm''; test (b24, s, ''a'');');
      add ('   s := ''mm''; test (b30, s, ''m'');');
      add ('   s := ''mm''; test (b36, s, ''z'');');
      add ('end.');
      start_test (81);
      test_abs_value (0+0, ord(''='m'));
      test_abs_value (0+1, ord(''<>'m'));
      test_abs_value (0+2, ord(''<'m'));
      test_abs_value (0+3, ord(''>'m'));
      test_abs_value (0+4, ord(''<='m'));
      test_abs_value (0+5, ord(''>='m'));
      test_abs_value (6+0, ord('a'='m'));
      test_abs_value (6+1, ord('a'<>'m'));
      test_abs_value (6+2, ord('a'<'m'));
      test_abs_value (6+3, ord('a'>'m'));
      test_abs_value (6+4, ord('a'<='m'));
      test_abs_value (6+5, ord('a'>='m'));
      test_abs_value (12+0, ord('m'='m'));
      test_abs_value (12+1, ord('m'<>'m'));
      test_abs_value (12+2, ord('m'<'m'));
      test_abs_value (12+3, ord('m'>'m'));
      test_abs_value (12+4, ord('m'<='m'));
      test_abs_value (12+5, ord('m'>='m'));
      test_abs_value (18+0, ord('z'='m'));
      test_abs_value (18+1, ord('z'<>'m'));
      test_abs_value (18+2, ord('z'<'m'));
      test_abs_value (18+3, ord('z'>'m'));
      test_abs_value (18+4, ord('z'<='m'));
      test_abs_value (18+5, ord('z'>='m'));
      test_abs_value (24+0, ord('mm'='a'));
      test_abs_value (24+1, ord('mm'<>'a'));
      test_abs_value (24+2, ord('mm'<'a'));
      test_abs_value (24+3, ord('mm'>'a'));
      test_abs_value (24+4, ord('mm'<='a'));
      test_abs_value (24+5, ord('mm'>='a'));
      test_abs_value (30+0, ord('mm'='m'));
      test_abs_value (30+1, ord('mm'<>'m'));
      test_abs_value (30+2, ord('mm'<'m'));
      test_abs_value (30+3, ord('mm'>'m'));
      test_abs_value (30+4, ord('mm'<='m'));
      test_abs_value (30+5, ord('mm'>='m'));
      test_abs_value (36+0, ord('mm'='z'));
      test_abs_value (36+1, ord('mm'<>'z'));
      test_abs_value (36+2, ord('mm'<'z'));
      test_abs_value (36+3, ord('mm'>'z'));
      test_abs_value (36+4, ord('mm'<='z'));
      test_abs_value (36+5, ord('mm'>='z'));
      conclude_test
   end;

procedure test82;  // compare rom str to char
   begin
      add ('type');
      add ('   ba = array [0..5] of boolean;');
      add ('procedure test (var b: ba; rom s: string; c: char);');
      add ('   begin');
      add ('      b[0] := s = c;');
      add ('      b[1] := s <> c;');
      add ('      b[2] := s < c;');
      add ('      b[3] := s > c;');
      add ('      b[4] := s <= c;');
      add ('      b[5] := s >= c');
      add ('   end;');
      add ('var');
      add ('   b0, b6, b12, b18, b24, b30, b36: ba;');
      add ('rom');
      add ('   r: string[2] = '''';');
      add ('   ra: string[2] = ''a'';');
      add ('   rm: string[1] = ''m'';');
      add ('   rz: string[1] = ''z'';');
      add ('   rmm: string[2] = ''mm'';');
      add ('begin');
      add ('   test (b0, r, ''m'');');
      add ('   test (b6, ra, ''m'');');
      add ('   test (b12, rm, ''m'');');
      add ('   test (b18, rz, ''m'');');
      add ('   test (b24, rmm, ''a'');');
      add ('   test (b30, rmm, ''m'');');
      add ('   test (b36, rmm, ''z'');');
      add ('end.');
      start_test (82);
      test_abs_value (0+0, ord(''='m'));
      test_abs_value (0+1, ord(''<>'m'));
      test_abs_value (0+2, ord(''<'m'));
      test_abs_value (0+3, ord(''>'m'));
      test_abs_value (0+4, ord(''<='m'));
      test_abs_value (0+5, ord(''>='m'));
      test_abs_value (6+0, ord('a'='m'));
      test_abs_value (6+1, ord('a'<>'m'));
      test_abs_value (6+2, ord('a'<'m'));
      test_abs_value (6+3, ord('a'>'m'));
      test_abs_value (6+4, ord('a'<='m'));
      test_abs_value (6+5, ord('a'>='m'));
      test_abs_value (12+0, ord('m'='m'));
      test_abs_value (12+1, ord('m'<>'m'));
      test_abs_value (12+2, ord('m'<'m'));
      test_abs_value (12+3, ord('m'>'m'));
      test_abs_value (12+4, ord('m'<='m'));
      test_abs_value (12+5, ord('m'>='m'));
      test_abs_value (18+0, ord('z'='m'));
      test_abs_value (18+1, ord('z'<>'m'));
      test_abs_value (18+2, ord('z'<'m'));
      test_abs_value (18+3, ord('z'>'m'));
      test_abs_value (18+4, ord('z'<='m'));
      test_abs_value (18+5, ord('z'>='m'));
      test_abs_value (24+0, ord('mm'='a'));
      test_abs_value (24+1, ord('mm'<>'a'));
      test_abs_value (24+2, ord('mm'<'a'));
      test_abs_value (24+3, ord('mm'>'a'));
      test_abs_value (24+4, ord('mm'<='a'));
      test_abs_value (24+5, ord('mm'>='a'));
      test_abs_value (30+0, ord('mm'='m'));
      test_abs_value (30+1, ord('mm'<>'m'));
      test_abs_value (30+2, ord('mm'<'m'));
      test_abs_value (30+3, ord('mm'>'m'));
      test_abs_value (30+4, ord('mm'<='m'));
      test_abs_value (30+5, ord('mm'>='m'));
      test_abs_value (36+0, ord('mm'='z'));
      test_abs_value (36+1, ord('mm'<>'z'));
      test_abs_value (36+2, ord('mm'<'z'));
      test_abs_value (36+3, ord('mm'>'z'));
      test_abs_value (36+4, ord('mm'<='z'));
      test_abs_value (36+5, ord('mm'>='z'));
      conclude_test
   end;

procedure test83;  // compare eeprom str to char
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('   ba = array [0..5] of boolean;');
      add ('   tc = class');
      add ('           eeprom s: string [5];');
      add ('         public');
      add ('           procedure test (var b: ba; ss: string; c: char);');
      add ('              begin');
      add ('                 s := ss;');
      add ('                 b[0] := s = c;');
      add ('                 b[1] := s <> c;');
      add ('                 b[2] := s < c;');
      add ('                 b[3] := s > c;');
      add ('                 b[4] := s <= c;');
      add ('                 b[5] := s >= c');
      add ('              end;');
      add ('           begin');
      add ('           end;');
      add ('var');
      add ('   b0, b6, b12, b18, b24, b30, b36: ba;');
      add ('   s: string[2];');
      add ('   c: tc;');
      add ('begin');
      add ('   s := ''''; c.test (b0, s, ''m'');');
      add ('   s := ''a''; c.test (b6, s, ''m'');');
      add ('   s := ''m''; c.test (b12, s, ''m'');');
      add ('   s := ''z''; c.test (b18, s, ''m'');');
      add ('   s := ''mm''; c.test (b24, s, ''a'');');
      add ('   s := ''mm''; c.test (b30, s, ''m'');');
      add ('   s := ''mm''; c.test (b36, s, ''z'');');
      add ('end.');
      start_test (83);
      test_abs_value (0+0, ord(''='m'));
      test_abs_value (0+1, ord(''<>'m'));
      test_abs_value (0+2, ord(''<'m'));
      test_abs_value (0+3, ord(''>'m'));
      test_abs_value (0+4, ord(''<='m'));
      test_abs_value (0+5, ord(''>='m'));
      test_abs_value (6+0, ord('a'='m'));
      test_abs_value (6+1, ord('a'<>'m'));
      test_abs_value (6+2, ord('a'<'m'));
      test_abs_value (6+3, ord('a'>'m'));
      test_abs_value (6+4, ord('a'<='m'));
      test_abs_value (6+5, ord('a'>='m'));
      test_abs_value (12+0, ord('m'='m'));
      test_abs_value (12+1, ord('m'<>'m'));
      test_abs_value (12+2, ord('m'<'m'));
      test_abs_value (12+3, ord('m'>'m'));
      test_abs_value (12+4, ord('m'<='m'));
      test_abs_value (12+5, ord('m'>='m'));
      test_abs_value (18+0, ord('z'='m'));
      test_abs_value (18+1, ord('z'<>'m'));
      test_abs_value (18+2, ord('z'<'m'));
      test_abs_value (18+3, ord('z'>'m'));
      test_abs_value (18+4, ord('z'<='m'));
      test_abs_value (18+5, ord('z'>='m'));
      test_abs_value (24+0, ord('mm'='a'));
      test_abs_value (24+1, ord('mm'<>'a'));
      test_abs_value (24+2, ord('mm'<'a'));
      test_abs_value (24+3, ord('mm'>'a'));
      test_abs_value (24+4, ord('mm'<='a'));
      test_abs_value (24+5, ord('mm'>='a'));
      test_abs_value (30+0, ord('mm'='m'));
      test_abs_value (30+1, ord('mm'<>'m'));
      test_abs_value (30+2, ord('mm'<'m'));
      test_abs_value (30+3, ord('mm'>'m'));
      test_abs_value (30+4, ord('mm'<='m'));
      test_abs_value (30+5, ord('mm'>='m'));
      test_abs_value (36+0, ord('mm'='z'));
      test_abs_value (36+1, ord('mm'<>'z'));
      test_abs_value (36+2, ord('mm'<'z'));
      test_abs_value (36+3, ord('mm'>'z'));
      test_abs_value (36+4, ord('mm'<='z'));
      test_abs_value (36+5, ord('mm'>='z'));
      conclude_test
   end;

   procedure test84;  // compare rom str to eeprom str
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('   tc=class');
      add ('         eeprom e: string[5];');
      add ('         var s: string[5];');
      add ('     public');
      add ('      procedure equal (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e = s');
      add ('         end;');
      add ('      procedure not_equal (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <> s');
      add ('         end;');
      add ('      procedure less (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e < s');
      add ('         end;');
      add ('      procedure greater (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e > s');
      add ('         end;');
      add ('      procedure lessorequal (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <= s');
      add ('         end;');
      add ('      procedure greaterorequal(var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e >= s');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('rom');
      add ('   s1: string [5] = ''abc'';');
      add ('   s2: string [5] = ''ab'';');
      add ('   s3: string [5] = ''ax'';');
      add ('   s4: string [5] = '''';');
      add ('var');
      add ('   c: tc;');
      add ('begin');
      add ('   c.equal (b0, s1, s1);');
      add ('   c.not_equal (b1, s1, s1);');
      add ('   c.less (b2, s1, s1);');
      add ('   c.greater (b3, s1, s1);');
      add ('   c.lessorequal (b4, s1, s1);');
      add ('   c.greaterorequal (b5, s1, s1);');
      add ('   c.equal (b6, s1, s2);');
      add ('   c.not_equal (b7, s1, s2);');
      add ('   c.less (b8, s1, s2);');
      add ('   c.greater (b9, s1, s2);');
      add ('   c.lessorequal (b10, s1, s2);');
      add ('   c.greaterorequal (b11, s1, s2);');
      add ('   c.equal (b12, s2, s1);');
      add ('   c.not_equal (b13, s2, s1);');
      add ('   c.less (b14, s2, s1);');
      add ('   c.greater (b15, s2, s1);');
      add ('   c.lessorequal (b16, s2, s1);');
      add ('   c.greaterorequal (b17, s2, s1);');
      add ('   c.equal (b18, s1, s3);');
      add ('   c.not_equal (b19, s1, s3);');
      add ('   c.less (b20, s1, s3);');
      add ('   c.greater (b21, s1, s3);');
      add ('   c.lessorequal (b22, s1, s3);');
      add ('   c.greaterorequal (b23, s1, s3);');
      add ('   c.equal (b24, s3, s2);');
      add ('   c.not_equal (b25, s3, s2);');
      add ('   c.less (b26, s3, s2);');
      add ('   c.greater (b27, s3, s2);');
      add ('   c.lessorequal (b28, s3, s2);');
      add ('   c.greaterorequal (b29, s3, s2);');
      add ('   c.equal (b30, s1, s4);');
      add ('   c.not_equal (b31, s1, s4);');
      add ('   c.less (b32, s1, s4);');
      add ('   c.greater (b33, s1, s4);');
      add ('   c.lessorequal (b34, s1, s4);');
      add ('   c.greaterorequal (b35, s1, s4);');
      add ('   c.equal (b36, s4, s2);');
      add ('   c.not_equal (b37, s4, s2);');
      add ('   c.less (b38, s4, s2);');
      add ('   c.greater (b39, s4, s2);');
      add ('   c.lessorequal (b40, s4, s2);');
      add ('   c.greaterorequal (b41, s4, s2);');
      add ('end.');
      start_test (84);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test85;
   const
      s1 = 'abc';
      s2 = 'ab';
      s3 = 'ax';
      s4 = '';
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('   tc=class');
      add ('         eeprom e: string[5];');
      add ('           s: string[5];');
      add ('     public');
      add ('      procedure equal (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e = s');
      add ('         end;');
      add ('      procedure not_equal (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <> s');
      add ('         end;');
      add ('      procedure less (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e < s');
      add ('         end;');
      add ('      procedure greater (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e > s');
      add ('         end;');
      add ('      procedure lessorequal (var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e <= s');
      add ('         end;');
      add ('      procedure greaterorequal(var b: boolean; rom s1, s2: string);');
      add ('         begin');
      add ('            e := s1;');
      add ('            s := s2;');
      add ('            b := e >= s');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('var');
      add ('   b0,b1,b2,b3,b4,b5,b6,b7,b8,b9: boolean;');
      add ('   b10,b11,b12,b13,b14,b15,b16,b17,b18,b19: boolean;');
      add ('   b20,b21,b22,b23,b24,b25,b26,b27,b28,b29: boolean;');
      add ('   b30,b31,b32,b33,b34,b35,b36,b37,b38,b39: boolean;');
      add ('   b40,b41,b42,b43,b44,b45,b46,b47,b48,b49: boolean;');
      add ('rom');
      add ('   s1: string [5] = ''abc'';');
      add ('   s2: string [5] = ''ab'';');
      add ('   s3: string [5] = ''ax'';');
      add ('   s4: string [5] = '''';');
      add ('var');
      add ('   c: tc;');
      add ('begin');
      add ('   c.equal (b0, s1, s1);');
      add ('   c.not_equal (b1, s1, s1);');
      add ('   c.less (b2, s1, s1);');
      add ('   c.greater (b3, s1, s1);');
      add ('   c.lessorequal (b4, s1, s1);');
      add ('   c.greaterorequal (b5, s1, s1);');
      add ('   c.equal (b6, s1, s2);');
      add ('   c.not_equal (b7, s1, s2);');
      add ('   c.less (b8, s1, s2);');
      add ('   c.greater (b9, s1, s2);');
      add ('   c.lessorequal (b10, s1, s2);');
      add ('   c.greaterorequal (b11, s1, s2);');
      add ('   c.equal (b12, s2, s1);');
      add ('   c.not_equal (b13, s2, s1);');
      add ('   c.less (b14, s2, s1);');
      add ('   c.greater (b15, s2, s1);');
      add ('   c.lessorequal (b16, s2, s1);');
      add ('   c.greaterorequal (b17, s2, s1);');
      add ('   c.equal (b18, s1, s3);');
      add ('   c.not_equal (b19, s1, s3);');
      add ('   c.less (b20, s1, s3);');
      add ('   c.greater (b21, s1, s3);');
      add ('   c.lessorequal (b22, s1, s3);');
      add ('   c.greaterorequal (b23, s1, s3);');
      add ('   c.equal (b24, s3, s2);');
      add ('   c.not_equal (b25, s3, s2);');
      add ('   c.less (b26, s3, s2);');
      add ('   c.greater (b27, s3, s2);');
      add ('   c.lessorequal (b28, s3, s2);');
      add ('   c.greaterorequal (b29, s3, s2);');
      add ('   c.equal (b30, s1, s4);');
      add ('   c.not_equal (b31, s1, s4);');
      add ('   c.less (b32, s1, s4);');
      add ('   c.greater (b33, s1, s4);');
      add ('   c.lessorequal (b34, s1, s4);');
      add ('   c.greaterorequal (b35, s1, s4);');
      add ('   c.equal (b36, s4, s2);');
      add ('   c.not_equal (b37, s4, s2);');
      add ('   c.less (b38, s4, s2);');
      add ('   c.greater (b39, s4, s2);');
      add ('   c.lessorequal (b40, s4, s2);');
      add ('   c.greaterorequal (b41, s4, s2);');
      add ('end.');
      start_test (85);
      test_abs_value (0, ord(s1 = s1));
      test_abs_value (1, ord(s1 <> s1));
      test_abs_value (2, ord(s1 < s1));
      test_abs_value (3, ord(s1 > s1));
      test_abs_value (4, ord(s1 <= s1));
      test_abs_value (5, ord(s1 >= s1));
      test_abs_value (6, ord(s1 = s2));
      test_abs_value (7, ord(s1 <> s2));
      test_abs_value (8, ord(s1 < s2));
      test_abs_value (9, ord(s1 > s2));
      test_abs_value (10, ord(s1 <= s2));
      test_abs_value (11, ord(s1 >= s2));
      test_abs_value (12, ord(s2 = s1));
      test_abs_value (13, ord(s2 <> s1));
      test_abs_value (14, ord(s2 < s1));
      test_abs_value (15, ord(s2 > s1));
      test_abs_value (16, ord(s2 <= s1));
      test_abs_value (17, ord(s2 >= s1));
      test_abs_value (18, ord(s1 = s3));
      test_abs_value (19, ord(s1 <> s3));
      test_abs_value (20, ord(s1 < s3));
      test_abs_value (21, ord(s1 > s3));
      test_abs_value (22, ord(s1 <= s3));
      test_abs_value (23, ord(s1 >= s3));
      test_abs_value (24, ord(s3 = s2));
      test_abs_value (25, ord(s3 <> s2));
      test_abs_value (26, ord(s3 < s2));
      test_abs_value (27, ord(s3 > s2));
      test_abs_value (28, ord(s3 <= s2));
      test_abs_value (29, ord(s3 >= s2));
      test_abs_value (30, ord(s1 = s4));
      test_abs_value (31, ord(s1 <> s4));
      test_abs_value (32, ord(s1 < s4));
      test_abs_value (33, ord(s1 > s4));
      test_abs_value (34, ord(s1 <= s4));
      test_abs_value (35, ord(s1 >= s4));
      test_abs_value (36, ord(s4 = s2));
      test_abs_value (37, ord(s4 <> s2));
      test_abs_value (38, ord(s4 < s2));
      test_abs_value (39, ord(s4 > s2));
      test_abs_value (40, ord(s4 <= s2));
      test_abs_value (41, ord(s4 >= s2));
      conclude_test
   end;

procedure test86;
   begin
//      add ('begin');
//      add ('   test (1, ''running_prio 1'');');
//      add ('end.');
//      start_test (86);
//      conclude_test
   end;

procedure test87;       // test read of packed record
   begin
      add ('var');
      add ('   i,j,k,l,m,n,o: int8;');
      add ('   ii,jj,kk,ll: int16;');
      add ('   x: packed record');
      add ('         i1: int3;');
      add ('         i2: int5');
      add ('      end := (i1 = 2, i2 = 3);');
      add ('   y: packed record');
      add ('         i1: int3;');
      add ('         i2: int5');
      add ('      end := (i1 = -2, i2 = -3);');
      add ('   z: packed record');
      add ('         i1: int4;');
      add ('         i2: int6;');
      add ('         i3: int4');
      add ('      end := (i1 = -2, i2 = -3, i3 = -4);');
      add ('   zz: packed record');
      add ('         i1: int3;');
      add ('         i2: int15;');
      add ('         i3: int5');
      add ('      end := (i1 = -2, i2 = -3, i3 = -4);');
      add ('   zz2: packed record');
      add ('         i1: int3;');
      add ('         i2: int15;');
      add ('         i3: int5');
      add ('      end := (i1 = 2, i2 = 3, i3 = 4);');
      add ('begin');
      add (' i := x.i1;');
      add (' j := x.i2;');
      add (' k := y.i1;');
      add (' l := y.i2;');
      add (' m := z.i1;');
      add (' n := z.i2;');
      add (' o := z.i3;');
      add (' ii := zz.i2;');
      add (' jj := zz.i3;');
      add (' kk := zz2.i2;');
      add (' ll := zz2.i3;');
      add ('end.');
      start_test (87);
      test_abs_value (0, 2);
      test_abs_value (1, 3);
      test_abs_value (2, $fe);
      test_abs_value (3, $fd);
      test_abs_value (4, $fe);
      test_abs_value (5, $fd);
      test_abs_value (6, $fc);
      test_abs_value (7, $ff, $fd);
      test_abs_value (9, $ff, $fc);
      test_abs_value (11, 0, 3);
      test_abs_value (13, 0, 4);
      conclude_test
   end;

procedure test88;    // test write to packed record with constants
   begin
      add ('var');
      add (' adc: packed record');
      add ('        i: uint16');
      add ('      end;');
      add (' x: packed record');
      add ('      i: int2; j: int3; k: int3');
      add ('    end := (i=1, j=2, k=3);');
      add (' pr1,pr2,pr3,pr4: packed record');
      add ('      a: int3; b: int4; c: int10; d: int3');
      add ('    end := (a=1, b=2, c=3, d=3);');
      add ('begin');
      add ('   adc.i := 5;');
      add ('   x.j := -1;');
      add ('   pr1.a := -2;');
      add ('   pr2.b := -2;');
      add ('   pr3.c := -2;');
      add ('   pr4.d := -2;');
      add ('end.');
      start_test (88);
      test_abs_value (0, 0, 5);
      test_abs_value (2, $7B);
      test_abs_value (3, $c4, 1, $b0);
      test_abs_value (6, $3c, 1, $b0);
      test_abs_value (9, $25, $ff, $30);
      test_abs_value (12, $24, 1, $e0);
//      test_abs_value (3, $b0, 1, $c4);
//      test_abs_value (6, $b0, 1, $3c);
//      test_abs_value (9, $30, $ff, $25);
//      test_abs_value (12, $e0, 1, $24);
      conclude_test
   end;

procedure test89;    // test write to packed record with variable
   begin
      add ('var');
      add (' adc: packed record');
      add ('        i: uint16');
      add ('      end;');
      add (' x: packed record');
      add ('      i: int2; j: int3; k: int3');
      add ('    end := (i=1, j=2, k=3);');
      add (' pr1,pr2,pr3,pr4: packed record');
      add ('      a: int3; b: int4; c: int10; d: int3');
      add ('    end := (a=1, b=2, c=3, d=3);');
      add (' i: int16;');
      add ('begin');
      add ('   i := 5; adc.i := i;');
      add ('   i := -1; x.j := i;');
      add ('   i := -2; pr1.a := i;');
      add ('   pr2.b := i;');
      add ('   pr3.c := i;');
      add ('   pr4.d := i;');
      add ('end.');
      start_test (89);
      test_abs_value (0, 0, 5);
      test_abs_value (2, $7B);
      test_abs_value (3, $c4, 1, $b0);
      test_abs_value (6, $3c, 1, $b0);
      test_abs_value (9, $25, $ff, $30);
      test_abs_value (12, $24, 1, $e0);
      conclude_test
   end;

procedure test90;    // test write to eeprom packed record with constants
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add (' tpr = packed record');
      add ('      a: int3; b: int4; c: int10; d: int3');
      add ('    end;');
      add ('var');
      add ('    pr1,pr2,pr3,pr4: tpr;');
      add (' c: class');
      add ('   eeprom pr1,pr2,pr3,pr4: tpr := (a=1, b=2, c=3, d=3);');
      add ('   public');
      add ('   procedure p (var p1,p2,p3,p4: tpr);');
      add ('      begin');
      add ('        p1 := pr1;');
      add ('        p2 := pr2;');
      add ('        p3 := pr3;');
      add ('        p4 := pr4;');
      add ('      end;');
      add ('   begin');
      add ('      pr1.a := -2;');
      add ('      pr2.b := -2;');
      add ('      pr3.c := -2;');
      add ('      pr4.d := -2;');
      add ('   end;');
      add ('begin');
      add ('   init c;');
      add ('   c.p (pr1, pr2, pr3, pr4);');
      add ('end.');
      start_test (90);
      test_abs_value (0, $c4, 1, $b0);
      test_abs_value (3, $3c, 1, $b0);
      test_abs_value (6, $25, $ff, $30);
      test_abs_value (9, $24, 1, $e0);
      conclude_test
   end;

procedure test91;  // write single bit non-eeprom packed record fields
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   x: packed record');
      add ('         i: 0..1;');
      add ('         j: 0..1;');
      add ('         k: 0..1;');
      add ('         l: 0..1');
      add ('      end;');
      add ('   i: int8;');
      add ('begin');
      add ('   x.i := 0;');
      add ('   x.j := 1;');
      add ('   i := 0; x.k := i;');
      add ('   i := 1; x.l := i');
      add ('end.');
      start_test (91);
      test_abs_value (0, $50);
      conclude_test
   end;

procedure test92;     // test r/w with record fields (packed and unpacked)
   begin
      add ('var');
      add (' y: record');
      add ('      i,j,k: int8');
      add ('    end := (i=1,j=2,k=3);');
      add (' x: packed record');
      add ('      i: int8;');
      add ('      j: int4;');
      add ('      k: int12');
      add ('    end := (i=4,j=5,k=6);');
      add (' ii,jj, t1: int8;');
      add ('begin');
      add (' t1 := 6;');
      add (' with y do');
      add ('    begin');
      add ('      ii := j;');
      add ('      j := t1');
      add ('    end;');
      add ('t1 := 7;');
      add (' with x do');
      add ('   begin');
      add ('    jj := j;');
      add ('    j := t1');
      add ('   end;');
      add ('end.');
      start_test (92);
      test_abs_value (0, 1, 6, 3);
      test_abs_value (3, 4, $70, 6);
      test_abs_value (6, 2);
      test_abs_value (7, 5);
      conclude_test
   end;

procedure test93;  // test 1 bit packed fields, signed & unsigned
   begin
      add ('var');
      add (' pr1,pr2:');
      add ('     packed record');
      add ('       i: int1;');
      add ('       u: uint1');
      add ('     end;');
      add (' i,j,k,l: int8;');
      add ('begin');
      add (' i := -1;');
      add (' pr1.i := i;');
      add (' i := 1;');
      add (' pr1.u := i;');
      add (' i := 0;');
      add (' pr2.i := i;');
      add (' pr2.u := i;');
      add (' i := pr1.i;');
      add (' j := pr1.u;');
      add (' k := pr2.i;');
      add (' l := pr2.u;');
      add ('end.');
      start_test (93);
      test_abs_value (0, $c0);
      test_abs_value (1, 0);
      test_abs_value (2, $ff);
      test_abs_value (3, 1);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      conclude_test
   end;

procedure test94;     // test if-test 1-bit booleans
   begin
      add ('type');
      add (' tpr = packed record');
      add ('          b: boolean;');
      add ('          x: uint7;');
      add ('          i: int8');
      add ('       end;');
      add ('function f (b: boolean): int8;');
      add ('  var pt: tpr := (b=true, x=$7F, i=1);');
      add ('  begin');
      add ('     pt.b := b;');
      add ('     if pt.b then');
      add ('        result := 1');
      add ('     else');
      add ('        result := 0');
      add ('  end;');
      add ('function f2 (b: boolean; var pt: tpr): int8;');
      add ('  begin');
      add ('     pt.b := b;');
      add ('     if pt.b then');
      add ('        result := 1');
      add ('     else');
      add ('        result := 0');
      add ('  end;');
      add ('var');
      add ('  i0,i1,i2,i3,i4,i5,i6,i7: int8;');
      add ('  pr: tpr := (b=true, x=$7F, i=1);');
      add ('  b: boolean;');
      add ('begin');
      add ('  i0 := f(true);');
      add ('  i1 := f(false);');
      add ('  i2 := f2(true, pr);');
      add ('  i3 := f2(false, pr);');
      add ('  pr.b := true;');
      add ('  if pr.b then');
      add ('     i4 := 1');
      add ('  else');
      add ('     i4 := 0;');
      add ('  pr.b := false;');
      add ('  if pr.b then');
      add ('     i5 := 1');
      add ('  else');
      add ('     i5 := 0;');
      add ('  b := true;');
      add ('  if b then');
      add ('     i6 := 1');
      add ('  else');
      add ('     i6 := 0;');
      add ('  b := false;');
      add ('  if b then');
      add ('     i7 := 1');
      add ('  else');
      add ('     i7 := 0;');
      add ('end.');
      start_test (94);
      test_abs_value (0, 1);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 0);
      test_abs_value (4, 1);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      test_abs_value (7, 0);
      conclude_test
   end;

procedure test95;
   begin
      add ('var');
      add ('   i0: int8;');
      add ('   i1: int8;');
      add ('   i2: int8;');
      add ('   i3: int8;');
      add ('   i4: int8;');
      add ('   i5: int8;');
      add ('   i6: int8;');
      add ('   i7: int8;');
      add ('   i8: int8;');
      add ('   i9: int8;');
      add ('   i10: int8;');
      add ('   i11: int8;');
      add ('   i12: int8;');
      add ('   i13: int8;');
      add ('   i14: int8;');
      add ('   i15: int8;');
      add ('   i16: int8;');
      add ('   i17: int8;');
      add ('   i18: int8;');
      add ('   i19: int8;');
      add ('   i20: int8;');
      add ('   i21: int8;');
      add ('   i22: int8;');
      add ('   i23: int8;');
      add ('   i24: int8;');
      add ('   i25: int8;');
      add ('   i26: int8;');
      add ('   i27: int8;');
      add ('   pr: packed record b: 0..1 end;');
      add ('begin');
      add ('   pr.b := 0; if pr.b = 0 then i0 := 1 else i0 := 0;');
      add ('   pr.b := 0; if pr.b <> 0 then i1 := 1 else i1 := 0;');
      add ('   pr.b := 0; if pr.b < 0 then i2 := 1 else i2 := 0;');
      add ('   pr.b := 0; if pr.b > 0 then i3 := 1 else i3 := 0;');
      add ('   pr.b := 0; if pr.b <= 0 then i4 := 1 else i4 := 0;');
      add ('   pr.b := 0; if pr.b >= 0 then i5 := 1 else i5 := 0;');
      add ('   pr.b := 0; if pr.b in [] then i6 := 1 else i6 := 0;');
      add ('   pr.b := 0; if pr.b = 1 then i7 := 1 else i7 := 0;');
      add ('   pr.b := 0; if pr.b <> 1 then i8 := 1 else i8 := 0;');
      add ('   pr.b := 0; if pr.b < 1 then i9 := 1 else i9 := 0;');
      add ('   pr.b := 0; if pr.b > 1 then i10 := 1 else i10 := 0;');
      add ('   pr.b := 0; if pr.b <= 1 then i11 := 1 else i11 := 0;');
      add ('   pr.b := 0; if pr.b >= 1 then i12 := 1 else i12 := 0;');
      add ('   pr.b := 0; if pr.b in [0] then i13 := 1 else i13 := 0;');
      add ('   pr.b := 1; if pr.b = 0 then i14 := 1 else i14 := 0;');
      add ('   pr.b := 1; if pr.b <> 0 then i15 := 1 else i15 := 0;');
      add ('   pr.b := 1; if pr.b < 0 then i16 := 1 else i16 := 0;');
      add ('   pr.b := 1; if pr.b > 0 then i17 := 1 else i17 := 0;');
      add ('   pr.b := 1; if pr.b <= 0 then i18 := 1 else i18 := 0;');
      add ('   pr.b := 1; if pr.b >= 0 then i19 := 1 else i19 := 0;');
      add ('   pr.b := 1; if pr.b in [] then i20 := 1 else i20 := 0;');
      add ('   pr.b := 1; if pr.b = 1 then i21 := 1 else i21 := 0;');
      add ('   pr.b := 1; if pr.b <> 1 then i22 := 1 else i22 := 0;');
      add ('   pr.b := 1; if pr.b < 1 then i23 := 1 else i23 := 0;');
      add ('   pr.b := 1; if pr.b > 1 then i24 := 1 else i24 := 0;');
      add ('   pr.b := 1; if pr.b <= 1 then i25 := 1 else i25 := 0;');
      add ('   pr.b := 1; if pr.b >= 1 then i26 := 1 else i26 := 0;');
      add ('   pr.b := 1; if pr.b in [0] then i27 := 1 else i27 := 0;');
      add ('end.');
      start_test (95);
      test_abs_value (0, ord(0 = 0));
      test_abs_value (1, ord(0 <> 0));
      test_abs_value (2, ord(0 < 0));
      test_abs_value (3, ord(0 > 0));
      test_abs_value (4, ord(0 <= 0));
      test_abs_value (5, ord(0 >= 0));
      test_abs_value (6, ord(0 in []));
      test_abs_value (7, ord(0 = 1));
      test_abs_value (8, ord(0 <> 1));
      test_abs_value (9, ord(0 < 1));
      test_abs_value (10, ord(0 > 1));
      test_abs_value (11, ord(0 <= 1));
      test_abs_value (12, ord(0 >= 1));
      test_abs_value (13, ord(0 in [0]));
      test_abs_value (14, ord(1 = 0));
      test_abs_value (15, ord(1 <> 0));
      test_abs_value (16, ord(1 < 0));
      test_abs_value (17, ord(1 > 0));
      test_abs_value (18, ord(1 <= 0));
      test_abs_value (19, ord(1 >= 0));
      test_abs_value (20, ord(1 in []));
      test_abs_value (21, ord(1 = 1));
      test_abs_value (22, ord(1 <> 1));
      test_abs_value (23, ord(1 < 1));
      test_abs_value (24, ord(1 > 1));
      test_abs_value (25, ord(1 <= 1));
      test_abs_value (26, ord(1 >= 1));
      test_abs_value (27, ord(1 in [0]));
      conclude_test
   end;

procedure test96;
   begin
      add ('var');
      add ('   i0: int8;');
      add ('   i1: int8;');
      add ('   i2: int8;');
      add ('   i3: int8;');
      add ('   i4: int8;');
      add ('   i5: int8;');
      add ('   i6: int8;');
      add ('   i7: int8;');
      add ('   i8: int8;');
      add ('   i9: int8;');
      add ('   i10: int8;');
      add ('   i11: int8;');
      add ('   i12: int8;');
      add ('   i13: int8;');
      add ('   i14: int8;');
      add ('   i15: int8;');
      add ('   i16: int8;');
      add ('   i17: int8;');
      add ('   i18: int8;');
      add ('   i19: int8;');
      add ('   i20: int8;');
      add ('   i21: int8;');
      add ('   i22: int8;');
      add ('   i23: int8;');
      add ('   pr: packed record b: 0..1 end;');
      add ('begin');
      add ('   pr.b := 0; if 0 = pr.b then i0 := 1 else i0 := 0;');
      add ('   pr.b := 0; if 0 <> pr.b then i1 := 1 else i1 := 0;');
      add ('   pr.b := 0; if 0 < pr.b then i2 := 1 else i2 := 0;');
      add ('   pr.b := 0; if 0 > pr.b then i3 := 1 else i3 := 0;');
      add ('   pr.b := 0; if 0 <= pr.b then i4 := 1 else i4 := 0;');
      add ('   pr.b := 0; if 0 >= pr.b then i5 := 1 else i5 := 0;');
      add ('   pr.b := 0; if 1 = pr.b then i6 := 1 else i6 := 0;');
      add ('   pr.b := 0; if 1 <> pr.b then i7 := 1 else i7 := 0;');
      add ('   pr.b := 0; if 1 < pr.b then i8 := 1 else i8 := 0;');
      add ('   pr.b := 0; if 1 > pr.b then i9 := 1 else i9 := 0;');
      add ('   pr.b := 0; if 1 <= pr.b then i10 := 1 else i10 := 0;');
      add ('   pr.b := 0; if 1 >= pr.b then i11 := 1 else i11 := 0;');
      add ('   pr.b := 1; if 0 = pr.b then i12 := 1 else i12 := 0;');
      add ('   pr.b := 1; if 0 <> pr.b then i13 := 1 else i13 := 0;');
      add ('   pr.b := 1; if 0 < pr.b then i14 := 1 else i14 := 0;');
      add ('   pr.b := 1; if 0 > pr.b then i15 := 1 else i15 := 0;');
      add ('   pr.b := 1; if 0 <= pr.b then i16 := 1 else i16 := 0;');
      add ('   pr.b := 1; if 0 >= pr.b then i17 := 1 else i17 := 0;');
      add ('   pr.b := 1; if 1 = pr.b then i18 := 1 else i18 := 0;');
      add ('   pr.b := 1; if 1 <> pr.b then i19 := 1 else i19 := 0;');
      add ('   pr.b := 1; if 1 < pr.b then i20 := 1 else i20 := 0;');
      add ('   pr.b := 1; if 1 > pr.b then i21 := 1 else i21 := 0;');
      add ('   pr.b := 1; if 1 <= pr.b then i22 := 1 else i22 := 0;');
      add ('   pr.b := 1; if 1 >= pr.b then i23 := 1 else i23 := 0;');
      add ('end.');
      start_test (96);
      test_abs_value (0, ord(0 = 0));
      test_abs_value (1, ord(0 <> 0));
      test_abs_value (2, ord(0 < 0));
      test_abs_value (3, ord(0 > 0));
      test_abs_value (4, ord(0 <= 0));
      test_abs_value (5, ord(0 >= 0));
      test_abs_value (6, ord(1 = 0));
      test_abs_value (7, ord(1 <> 0));
      test_abs_value (8, ord(1 < 0));
      test_abs_value (9, ord(1 > 0));
      test_abs_value (10, ord(1 <= 0));
      test_abs_value (11, ord(1 >= 0));
      test_abs_value (12, ord(0 = 1));
      test_abs_value (13, ord(0 <> 1));
      test_abs_value (14, ord(0 < 1));
      test_abs_value (15, ord(0 > 1));
      test_abs_value (16, ord(0 <= 1));
      test_abs_value (17, ord(0 >= 1));
      test_abs_value (18, ord(1 = 1));
      test_abs_value (19, ord(1 <> 1));
      test_abs_value (20, ord(1 < 1));
      test_abs_value (21, ord(1 > 1));
      test_abs_value (22, ord(1 <= 1));
      test_abs_value (23, ord(1 >= 1));
      conclude_test
   end;

procedure test97;     // test strpos ram string in ram string
   begin
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6: uint8 := 100;');
      add ('   str: string[15];');
      add ('   substr: string[15];');
      add ('begin');
      add ('   i0 := str.strpos(substr);');
      add ('   str := ''abcdef'';');
      add ('   i1 := str.strpos(substr);');
      add ('   substr := ''ab'';');
      add ('   i2 := str.strpos(substr);');
      add ('   substr := ''cd'';');
      add ('   i3 := str.strpos(substr);');
      add ('   substr := ''xy'';');
      add ('   i4 := str.strpos(substr);');
      add ('   substr := ''xxxxxxxxx'';');
      add ('   i5 := str.strpos(substr);');
      add ('   substr := str;');
      add ('   i6 := str.strpos(substr);');
      add ('end.');
      start_test (97);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      conclude_test
   end;

procedure test98;    // test strpos rom string in ram string
   begin
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8 := 100;');
      add ('   str: string[15];');
      add ('rom');
      add ('   substr0: string[1] = '''';');
      add ('   substr1: string[2] = ''ab'';');
      add ('   substr2: string[2] = ''cd'';');
      add ('   substr3: string[2] = ''xy'';');
      add ('   substr4: string[9] = ''xxxxxxxxx'';');
      add ('   substr5: string[6] = ''abcdef'';');
      add ('begin');
      add ('   i0 := str.strpos(substr1);');
      add ('   str := ''abcdef'';');
      add ('   i1 := str.strpos(substr0);');
      add ('   i2 := str.strpos(substr1);');
      add ('   i3 := str.strpos(substr2);');
      add ('   i4 := str.strpos(substr3);');
      add ('   i5 := str.strpos(substr4);');
      add ('   i6 := str.strpos(substr5);');
      add ('   i7 := str.strpos(''abcdef'');');
      add ('end.');
      start_test (98);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      conclude_test
   end;

procedure test99;    // test strpos eeprom string in ram string
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6: uint8 := 100;');
      add ('   c: class');
      add ('      eeprom');
      add ('         substr: string[15];');
      add ('      public');
      add ('      function f (s, ss: string): uint8;');
      add ('         var str: string [20];');
      add ('         begin');
      add ('            substr := ss;');
      add ('            result := s.strpos (substr)');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('   str: string[10];');
      add ('   substr: string [10];');
      add ('   begin');
      add ('      i0 := c.f (str, substr);');
      add ('      str := ''abcdef'';');
      add ('      i1 := c.f (str, substr);');
      add ('      substr := ''ab'';');
      add ('      i2 := c.f (str, substr);');
      add ('      substr := ''cd'';');
      add ('      i3 := c.f (str, substr);');
      add ('      substr := ''xy'';');
      add ('      i4 := c.f (str, substr);');
      add ('      substr := ''xxxxxxxxx'';');
      add ('      i5 := c.f (str, substr);');
      add ('      substr := str;');
      add ('      i6 := c.f (str, substr);');
      add ('   end.');
      start_test (99);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      conclude_test
   end;

procedure test100;
   begin
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6: uint8 := 100;');
      add ('   substr: string[15];');
      add ('rom');
      add ('   str0: string[1] = '''';');
      add ('   str: string[15] = ''abcdef'';');
      add ('begin');
      add ('   i0 := str0.strpos(substr);');
      add ('   i1 := str.strpos(substr);');
      add ('   substr := ''ab'';');
      add ('   i2 := str.strpos(substr);');
      add ('   substr := ''cd'';');
      add ('   i3 := str.strpos(substr);');
      add ('   substr := ''xy'';');
      add ('   i4 := str.strpos(substr);');
      add ('   substr := ''xxxxxxxxx'';');
      add ('   i5 := str.strpos(substr);');
      add ('   substr := str;');
      add ('   i6 := str.strpos(substr);');
      add ('end.');
      start_test (100);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      conclude_test
   end;

procedure test101;
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6: uint8 := 100;');
      add ('   substr: string[15];');
      add ('   c: class');
      add ('         eeprom');
      add ('            substr: string[10];');
      add ('      public');
      add ('      function f (rom str: string; s: string): uint8;');
      add ('         begin');
      add ('            substr := s;');
      add ('            result := str.strpos(substr)');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('rom');
      add ('   str0: string[1] = '''';');
      add ('   str: string[15] = ''abcdef'';');
      add ('begin');
      add ('   i0 := c.f (str0, substr);');
      add ('   i1 := c.f (str, substr);');
      add ('   substr := ''ab'';');
      add ('   i2 := c.f (str, substr);');
      add ('   substr := ''cd'';');
      add ('   i3 := c.f (str, substr);');
      add ('   substr := ''xy'';');
      add ('   i4 := c.f (str, substr);');
      add ('   substr := ''xxxxxxxxx'';');
      add ('   i5 := c.f (str, substr);');
      add ('   substr := str;');
      add ('   i6 := c.f (str, substr);');
      add ('end.');
      start_test (101);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      conclude_test
   end;

procedure test102;
   begin
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8 := 100;');
      add ('rom');
      add ('   str0: string[1] = '''';');
      add ('   str: string[10] = ''abcdef'';');
      add ('   substr0: string[1] = '''';');
      add ('   substr1: string[2] = ''ab'';');
      add ('   substr2: string[2] = ''cd'';');
      add ('   substr3: string[2] = ''xy'';');
      add ('   substr4: string[9] = ''xxxxxxxxx'';');
      add ('   substr5: string[6] = ''abcdef'';');
      add ('begin');
      add ('   i0 := str0.strpos(substr1);');
      add ('   i1 := str.strpos(substr0);');
      add ('   i2 := str.strpos(substr1);');
      add ('   i3 := str.strpos(substr2);');
      add ('   i4 := str.strpos(substr3);');
      add ('   i5 := str.strpos(substr4);');
      add ('   i6 := str.strpos(substr5);');
      add ('   i7 := str.strpos(''abcdef'');');
      add ('end.');
      start_test (102);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      conclude_test
   end;

procedure test103;    // test strpos ram string in eeprom string
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6: uint8 := 100;');
      add ('   c: class');
      add ('      eeprom');
      add ('         es: string[15];');
      add ('      public');
      add ('      function f (s, ss: string): uint8;');
      add ('         var str: string [20];');
      add ('         begin');
      add ('            es := s;');
      add ('            result := es.strpos (ss)');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('   str: string[10];');
      add ('   substr: string [10];');
      add ('begin');
      add ('   i0 := c.f (str, substr);');
      add ('   str := ''abcdef'';');
      add ('   i1 := c.f (str, substr);');
      add ('   substr := ''ab'';');
      add ('   i2 := c.f (str, substr);');
      add ('   substr := ''cd'';');
      add ('   i3 := c.f (str, substr);');
      add ('   substr := ''xy'';');
      add ('   i4 := c.f (str, substr);');
      add ('   substr := ''xxxxxxxxx'';');
      add ('   i5 := c.f (str, substr);');
      add ('   substr := str;');
      add ('   i6 := c.f (str, substr);');
      add ('end.');
      start_test (103);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      conclude_test
   end;

procedure test104;    // test strpos rom string in eeprom string
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8 := 100;');
      add ('   c: class');
      add ('      eeprom');
      add ('         es: string[15];');
      add ('     public');
      add ('      function f (s: string; rom ss: string): uint8;');
      add ('         var str: string [20];');
      add ('         begin');
      add ('            es := s;');
      add ('            result := es.strpos (ss)');
      add ('         end;');
      add ('      function f2 (s: string): uint8;');
      add ('         begin');
      add ('            es := s;');
      add ('            result := es.strpos (''ef'');');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('   str: string[10];');
      add ('rom');
      add ('   substr0: string[1] = '''';');
      add ('   substr1: string[2] = ''ab'';');
      add ('   substr2: string[2] = ''cd'';');
      add ('   substr3: string[2] = ''xy'';');
      add ('   substr4: string[10] = ''xxxxxxxxx'';');
      add ('   substr5: string[10] = ''abcdef'';');
      add ('begin');
      add ('   i0 := c.f (str, substr1);');
      add ('   str := ''abcdef'';');
      add ('   i1 := c.f (str, substr0);');
      add ('   i2 := c.f (str, substr1);');
      add ('   i3 := c.f (str, substr2);');
      add ('   i4 := c.f (str, substr3);');
      add ('   i5 := c.f (str, substr4);');
      add ('   i6 := c.f (str, substr5);');
      add ('   i7 := c.f2 (str);');
      add ('end.');
      start_test (104);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      test_abs_value (7, 5);
      conclude_test
   end;

procedure test105;    // test strpos eeprom string in eeprom string
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6: uint8 := 100;');
      add ('   c: class');
      add ('      eeprom');
      add ('         es: string[15];');
      add ('         ess: string[10];');
      add ('     public');
      add ('      function f (s: string; rom ss: string): uint8;');
      add ('         var str: string [20];');
      add ('         begin');
      add ('            es := s;');
      add ('            ess := ss;');
      add ('            result := es.strpos (ess)');
      add ('         end;');
      add ('      begin');
      add ('      end;');
      add ('   str: string[10];');
      add ('rom');
      add ('   substr0: string[1] = '''';');
      add ('   substr1: string[2] = ''ab'';');
      add ('   substr2: string[2] = ''cd'';');
      add ('   substr3: string[2] = ''xy'';');
      add ('   substr4: string[10] = ''xxxxxxxxx'';');
      add ('   substr5: string[10] = ''abcdef'';');
      add ('begin');
      add ('   i0 := c.f (str, substr1);');
      add ('   str := ''abcdef'';');
      add ('   i1 := c.f (str, substr0);');
      add ('   i2 := c.f (str, substr1);');
      add ('   i3 := c.f (str, substr2);');
      add ('   i4 := c.f (str, substr3);');
      add ('   i5 := c.f (str, substr4);');
      add ('   i6 := c.f (str, substr5);');
      add ('end.');
      start_test (105);
      test_abs_value (0, 0);
      test_abs_value (1, 0);
      test_abs_value (2, 1);
      test_abs_value (3, 3);
      test_abs_value (4, 0);
      test_abs_value (5, 0);
      test_abs_value (6, 1);
      conclude_test
   end;

procedure test106;
   begin
      add ('var');
      add ('   i0,i1,i2,i3: int8;');
      add ('   s: string[10];');
      add ('   c: char;');
      add ('begin');
      add ('   i0 := s.strpos(c);');
      add ('   s := ''abcdef'';');
      add ('   c := ''a''; i1 := s.strpos (c);');
      add ('   c := ''c''; i2 := s.strpos (c);');
      add ('   c := ''x''; i3 := s.strpos (c);');
      add ('end.');
      start_test (106);
      test_abs_value (0, 0);
      test_abs_value (1, 1);
      test_abs_value (2, 3);
      test_abs_value (3, 0);
      conclude_test
   end;

procedure test107;
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   i0,i1,i2,i3: int8;');
      add ('   s: string[10];');
      add ('   c: class');
      add ('        eeprom es: string[10];');
      add ('       public');
      add ('        function f (s: string; c: char): uint8;');
      add ('           begin');
      add ('              es := s;');
      add ('              result := es.strpos(c)');
      add ('           end;');
      add ('        begin');
      add ('        end;');
      add ('begin');
      add ('   i0 := c.f (s, ''x'');');
      add ('   s := ''abcdef'';');
      add ('   i1 := c.f (s, ''a'');');
      add ('   i2 := c.f (s, ''c'');');
      add ('   i3 := c.f (s, ''x'');');
      add ('end.');
      start_test (107);
      test_abs_value (0, 0);
      test_abs_value (1, 1);
      test_abs_value (2, 3);
      test_abs_value (3, 0);
      conclude_test
   end;

procedure test108;
   begin
      add ('var');
      add ('   i0,i1,i2,i3: int8;');
      add ('   c: char;');
      add ('rom');
      add ('   s0: string[1] = '''';');
      add ('   s: string[10] = ''abcdef'';');
      add ('begin');
      add ('   i0 := s0.strpos(c);');
      add ('   c := ''a''; i1 := s.strpos (c);');
      add ('   c := ''c''; i2 := s.strpos (c);');
      add ('   c := ''x''; i3 := s.strpos (c);');
      add ('end.');
      start_test (108);
      test_abs_value (0, 0);
      test_abs_value (1, 1);
      test_abs_value (2, 3);
      test_abs_value (3, 0);
      conclude_test
   end;

procedure test109;    // test anonymous rom string parameters
   begin
      add ('var');
      add ('   s1: string[4];');
      add ('   s2: string[4];');
      add ('function f (rom s: string): string;');
      add ('   begin');
      add ('      result := s');
      add ('   end;');
      add ('begin');
      add ('  s1 := ''x''; s2 := ''z'';');
      add ('  s1 := f(''abc'');');
      add ('  s2 := f('''')');
      add ('end.');
      start_test (109);
      test_abs_value (0, 3, ord('a'), ord('b'), ord('c'));
      test_abs_value (5, 0);
      conclude_test
   end;

procedure test110;  // dropped cycle from reset timer routines
   begin
      add ('{$processor ''pic18f2520''}');
      add ('var');
      add ('   c: class (ioreg tmr3: tTMR16);');
      add ('       public');
      add ('         function f(i,c: uint16): uint24;');
      add ('            begin');
      add ('               TMR3.TMRx := i;');
      add ('               reset_tmr3_cycle (c);');
      add ('               result := ErrorCode');
      add ('            end;');
      add ('         begin');
      add ('         end;');
      add ('var');
      add (' err0: uint24;');
      add (' err3: uint24;');
      add (' err6: uint24;');
      add (' err9: uint24;');
      add ('begin');
      add ('  init c (tmr3);');
      add ('  TMR3.TMRx := 100;');
      add ('  reset_tmr3_cycle (1000);');
      add ('  err0 := ErrorCode;');
      add ('  TMR3.TMRx := $c000;');
      add ('  reset_tmr3_cycle (1000);');
      add ('  err3 := ErrorCode;');
      add ('  err6 := c.f (100, 1000);');
      add ('  err9 := c.f ($c000, 1000);');
      add ('end.');
      start_test (110);
      test_abs_value (0, 0);
      test_run_time_error_detected (3, err_timer_cycle_count_exceeded(3));
      test_abs_value (6, 0);
      test_run_time_error_detected (9, err_timer_cycle_count_exceeded(3));
      conclude_test
   end;

procedure test111;
   begin
      add ('{$processor ''pic18f65j94''}');
      add ('var');
      add ('   i0: uint11;');
      add ('   i2: uint16;');
      add ('begin');
      add ('   UFRM.FRM := $123;');   // normal order
      add ('   i0 := UFRM.FRM;');
      add ('   TMR3.TMRx := $1234;');  // reverse order
      add ('   i2 := TMR3.TMRx');
      add ('end.');
      start_test (111);
      test_sfr_value ('UFRMH', $01);
      test_sfr_value ('UFRML', $23);
      test_abs_value (0, $1, $23);
      test_sfr_value ('TMR3H', $12);
      test_sfr_value ('TMR3L', $34);
      test_abs_value (2, $12, $34);
      conclude_test
   end;

procedure test112;  // push single unsigned global bit
   begin
      add ('type');
      add ('  t=overlay');
      add ('       i: uint8;');
      add ('       packed record');
      add ('          i7: uint1;');
      add ('          i6: uint1;');
      add ('          i5: uint1;');
      add ('          i4: uint1;');
      add ('          i3: uint1;');
      add ('          i2: uint1;');
      add ('          i1: uint1;');
      add ('          i0: uint1');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8;');
      add ('   i8,i9,i10,i11,i12,i13,i14,i15: uint8;');
      add ('   v: t;');
      add ('begin');
      add ('   v.i := $80;');
      add ('   i0 := v.i7;');
      add ('   v.i := $40;');
      add ('   i1 := v.i6;');
      add ('   v.i := $20;');
      add ('   i2 := v.i5;');
      add ('   v.i := $10;');
      add ('   i3 := v.i4;');
      add ('   v.i := $08;');
      add ('   i4 := v.i3;');
      add ('   v.i := $04;');
      add ('   i5 := v.i2;');
      add ('   v.i := $02;');
      add ('   i6 := v.i1;');
      add ('   v.i := $01;');
      add ('   i7 := v.i0;');
      add ('   v.i := $7f;');
      add ('   i8 := v.i7;');
      add ('   v.i := $bf;');
      add ('   i9 := v.i6;');
      add ('   v.i := $df;');
      add ('   i10 := v.i5;');
      add ('   v.i := $ef;');
      add ('   i11 := v.i4;');
      add ('   v.i := $f7;');
      add ('   i12 := v.i3;');
      add ('   v.i := $fb;');
      add ('   i13 := v.i2;');
      add ('   v.i := $fd;');
      add ('   i14 := v.i1;');
      add ('   v.i := $fe;');
      add ('   i15 := v.i0;');
      add ('end.');
      start_test (112);
      test_abs_value (0, 1);
      test_abs_value (1, 1);
      test_abs_value (2, 1);
      test_abs_value (3, 1);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      conclude_test
   end;

procedure test113;   // push single unsigned local bit
   begin
      add ('type');
      add ('  t=overlay');
      add ('       i: uint8;');
      add ('       packed record');
      add ('          i7: uint1;');
      add ('          i6: uint1;');
      add ('          i5: uint1;');
      add ('          i4: uint1;');
      add ('          i3: uint1;');
      add ('          i2: uint1;');
      add ('          i1: uint1;');
      add ('          i0: uint1');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8;');
      add ('   i8,i9,i10,i11,i12,i13,i14,i15: uint8;');
      add ('function f0 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i0');
      add ('   end;');
      add ('function f1 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i1');
      add ('   end;');
      add ('function f2 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i2');
      add ('   end;');
      add ('function f3 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i3');
      add ('   end;');
      add ('function f4 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i4');
      add ('   end;');
      add ('function f5 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i5');
      add ('   end;');
      add ('function f6 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i6');
      add ('   end;');
      add ('function f7 (i: uint8): uint8;');
      add ('   var v: t;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i7');
      add ('   end;');
      add ('begin');
      add ('   i0 := f7($80);');
      add ('   i1 := f6($40);');
      add ('   i2 := f5($20);');
      add ('   i3 := f4($10);');
      add ('   i4 := f3($08);');
      add ('   i5 := f2($04);');
      add ('   i6 := f1($02);');
      add ('   i7 := f0($01);');
      add ('   i8 := f7($7f);');
      add ('   i9 := f6($bf);');
      add ('   i10 := f5($df);');
      add ('   i11 := f4($ef);');
      add ('   i12 := f3($f7);');
      add ('   i13 := f2($fb);');
      add ('   i14 := f1($fd);');
      add ('   i15 := f0($fe);');
      add ('end.');
      start_test (113);
      test_abs_value (0, 1);
      test_abs_value (1, 1);
      test_abs_value (2, 1);
      test_abs_value (3, 1);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      conclude_test
   end;

procedure test114;    // push single unsigned bit via F1
   begin
      add ('type');
      add ('  t=overlay');
      add ('       i: uint8;');
      add ('       packed record');
      add ('          i7: uint1;');
      add ('          i6: uint1;');
      add ('          i5: uint1;');
      add ('          i4: uint1;');
      add ('          i3: uint1;');
      add ('          i2: uint1;');
      add ('          i1: uint1;');
      add ('          i0: uint1');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8;');
      add ('   i8,i9,i10,i11,i12,i13,i14,i15: uint8;');
      add ('function f0 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i0');
      add ('   end;');
      add ('function f1 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;    ');
      add ('      result := v[j].i1');
      add ('   end;');
      add ('function f2 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i2');
      add ('   end;');
      add ('function f3 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i3');
      add ('   end;');
      add ('function f4 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i4');
      add ('   end;');
      add ('function f5 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i5');
      add ('   end;');
      add ('function f6 (i: uint8): uint8;');
      add ('   var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i6');
      add ('   end;');
      add ('function f7 (i: uint8): uint8;');
      add ('    var v: array [1..3] of t; j: int8;');
      add ('   begin');
      add ('      j := 2;');
      add ('      v[j].i := i;');
      add ('      result := v[j].i7');
      add ('   end;');
      add ('begin');
      add ('   i0 := f7($80);');
      add ('   i1 := f6($40);');
      add ('   i2 := f5($20);');
      add ('   i3 := f4($10);');
      add ('   i4 := f3($08);');
      add ('   i5 := f2($04);');
      add ('   i6 := f1($02);');
      add ('   i7 := f0($01);');
      add ('   i8 := f7($7f);');
      add ('   i9 := f6($bf);');
      add ('   i10 := f5($df);');
      add ('   i11 := f4($ef);');
      add ('   i12 := f3($f7);');
      add ('   i13 := f2($fb);');
      add ('   i14 := f1($fd);');
      add ('   i15 := f0($fe);');
      add ('end.');
      start_test (114);
      test_abs_value (0, 1);
      test_abs_value (1, 1);
      test_abs_value (2, 1);
      test_abs_value (3, 1);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      conclude_test
   end;

procedure test115;   // push single rom unsigned bit
   begin
      add ('type');
      add ('  t=packed record');
      add ('          i7: uint1;');
      add ('          i6: uint1;');
      add ('          i5: uint1;');
      add ('          i4: uint1;');
      add ('          i3: uint1;');
      add ('          i2: uint1;');
      add ('          i1: uint1;');
      add ('          i0: uint1');
      add ('   end;');
      add ('rom');
      add ('   r80: t = (i7=1,i6=0,i5=0,i4=0,i3=0,i2=0,i1=0,i0=0);');
      add ('   r40: t = (i7=0,i6=1,i5=0,i4=0,i3=0,i2=0,i1=0,i0=0);');
      add ('   r20: t = (i7=0,i6=0,i5=1,i4=0,i3=0,i2=0,i1=0,i0=0);');
      add ('   r10: t = (i7=0,i6=0,i5=0,i4=1,i3=0,i2=0,i1=0,i0=0);');
      add ('   r08: t = (i7=0,i6=0,i5=0,i4=0,i3=1,i2=0,i1=0,i0=0);');
      add ('   r04: t = (i7=0,i6=0,i5=0,i4=0,i3=0,i2=1,i1=0,i0=0);');
      add ('   r02: t = (i7=0,i6=0,i5=0,i4=0,i3=0,i2=0,i1=1,i0=0);');
      add ('   r01: t = (i7=0,i6=0,i5=0,i4=0,i3=0,i2=0,i1=0,i0=1);');
      add ('   r7f: t = (i7=0,i6=1,i5=1,i4=1,i3=1,i2=1,i1=1,i0=1);');
      add ('   rbf: t = (i7=1,i6=0,i5=1,i4=1,i3=1,i2=1,i1=1,i0=1);');
      add ('   rdf: t = (i7=1,i6=1,i5=0,i4=1,i3=1,i2=1,i1=1,i0=1);');
      add ('   ref: t = (i7=1,i6=1,i5=1,i4=0,i3=1,i2=1,i1=1,i0=1);');
      add ('   rf7: t = (i7=1,i6=1,i5=1,i4=1,i3=0,i2=1,i1=1,i0=1);');
      add ('   rfb: t = (i7=1,i6=1,i5=1,i4=1,i3=1,i2=0,i1=1,i0=1);');
      add ('   rfd: t = (i7=1,i6=1,i5=1,i4=1,i3=1,i2=1,i1=0,i0=1);');
      add ('   rfe: t = (i7=1,i6=1,i5=1,i4=1,i3=1,i2=1,i1=1,i0=0);');
      add ('function f0 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i0');
      add ('   end;');
      add ('function f1 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i1');
      add ('   end;');
      add ('function f2 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i2');
      add ('   end;');
      add ('function f3 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i3');
      add ('   end;');
      add ('function f4 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i4');
      add ('   end;');
      add ('function f5 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i5');
      add ('   end;');
      add ('function f6 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i6');
      add ('   end;');
      add ('function f7 (rom r: t): uint8;');
      add ('   begin');
      add ('      result := r.i7');
      add ('   end;');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8;');
      add ('   i8,i9,i10,i11,i12,i13,i14,i15: uint8;');
      add ('begin');
      add ('   i0 := f7(r80);');
      add ('   i1 := f6(r40);');
      add ('   i2 := f5(r20);');
      add ('   i3 := f4(r10);');
      add ('   i4 := f3(r08);');
      add ('   i5 := f2(r04);');
      add ('   i6 := f1(r02);');
      add ('   i7 := f0(r01);');
      add ('   i8 := f7(r7f);');
      add ('   i9 := f6(rbf);');
      add ('   i10 := f5(rdf);');
      add ('   i11 := f4(ref);');
      add ('   i12 := f3(rf7);');
      add ('   i13 := f2(rfb);');
      add ('   i14 := f1(rfd);');
      add ('   i15 := f0(rfe);');
      add ('end.');
      start_test (115);
      test_abs_value (0, 1);
      test_abs_value (1, 1);
      test_abs_value (2, 1);
      test_abs_value (3, 1);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      conclude_test
   end;

procedure test116;  // push single global signed bit
   begin
      add ('type');
      add ('  t=overlay');
      add ('       i: uint8;');
      add ('       packed record');
      add ('          i7: int1;');
      add ('          i6: int1;');
      add ('          i5: int1;');
      add ('          i4: int1;');
      add ('          i3: int1;');
      add ('          i2: int1;');
      add ('          i1: int1;');
      add ('          i0: int1');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: int8;');
      add ('   i8,i9,i10,i11,i12,i13,i14,i15: int8;');
      add ('   v: t;');
      add ('begin');
      add ('   v.i := $80;');
      add ('   i0 := v.i7;');
      add ('   v.i := $40;');
      add ('   i1 := v.i6;');
      add ('   v.i := $20;');
      add ('   i2 := v.i5;');
      add ('   v.i := $10;');
      add ('   i3 := v.i4;');
      add ('   v.i := $08;');
      add ('   i4 := v.i3;');
      add ('   v.i := $04;');
      add ('   i5 := v.i2;');
      add ('   v.i := $02;');
      add ('   i6 := v.i1;');
      add ('   v.i := $01;');
      add ('   i7 := v.i0;');
      add ('   v.i := $7f;');
      add ('   i8 := v.i7;');
      add ('   v.i := $bf;');
      add ('   i9 := v.i6;');
      add ('   v.i := $df;');
      add ('   i10 := v.i5;');
      add ('   v.i := $ef;');
      add ('   i11 := v.i4;');
      add ('   v.i := $f7;');
      add ('   i12 := v.i3;');
      add ('   v.i := $fb;');
      add ('   i13 := v.i2;');
      add ('   v.i := $fd;');
      add ('   i14 := v.i1;');
      add ('   v.i := $fe;');
      add ('   i15 := v.i0;');
      add ('end.');
      start_test (116);
      test_abs_value (0, $ff);
      test_abs_value (1, $ff);
      test_abs_value (2, $ff);
      test_abs_value (3, $ff);
      test_abs_value (4, $ff);
      test_abs_value (5, $ff);
      test_abs_value (6, $ff);
      test_abs_value (7, $ff);
      test_abs_value (8, 0);
      test_abs_value (9, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      conclude_test
   end;

procedure test117;  // push single eeprom unsigned bit
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('  t=overlay');
      add ('       i: uint8;');
      add ('       packed record');
      add ('          i7: uint1;');
      add ('          i6: uint1;');
      add ('          i5: uint1;');
      add ('          i4: uint1;');
      add ('          i3: uint1;');
      add ('          i2: uint1;');
      add ('          i1: uint1;');
      add ('          i0: uint1');
      add ('      end');
      add ('   end;');
      add ('var');
      add ('   i0,i1,i2,i3,i4,i5,i6,i7: uint8;');
      add ('   i8,i9,i10,i11,i12,i13,i14,i15: uint8;');
      add ('c: class');
      add ('      eeprom v: t;');
      add ('   public');
      add ('function f0 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i0');
      add ('   end;');
      add ('function f1 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i1');
      add ('   end;');
      add ('function f2 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i2');
      add ('   end;');
      add ('function f3 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i3');
      add ('   end;');
      add ('function f4 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i4');
      add ('   end;');
      add ('function f5 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i5');
      add ('   end;');
      add ('function f6 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i6');
      add ('   end;');
      add ('function f7 (i: uint8): uint8;');
      add ('   begin');
      add ('      v.i := i;');
      add ('      result := v.i7');
      add ('   end;');
      add ('begin end;');
      add ('begin');
      add ('   i0 := c.f7($80);');
      add ('   i1 := c.f6($40);');
      add ('   i2 := c.f5($20);');
      add ('   i3 := c.f4($10);');
      add ('   i4 := c.f3($08);');
      add ('   i5 := c.f2($04);');
      add ('   i6 := c.f1($02);');
      add ('   i7 := c.f0($01);');
      add ('   i8 := c.f7($7f);');
      add ('   i9 := c.f6($bf);');
      add ('   i10 := c.f5($df);');
      add ('   i11 := c.f4($ef);');
      add ('   i12 := c.f3($f7);');
      add ('   i13 := c.f2($fb);');
      add ('   i14 := c.f1($fd);');
      add ('   i15 := c.f0($fe);');
      add ('end.');
      start_test (117);
      test_abs_value (0, 1);
      test_abs_value (1, 1);
      test_abs_value (2, 1);
      test_abs_value (3, 1);
      test_abs_value (4, 1);
      test_abs_value (5, 1);
      test_abs_value (6, 1);
      test_abs_value (7, 1);
      test_abs_value (8, 0);
      test_abs_value (9, 0);
      test_abs_value (10, 0);
      test_abs_value (11, 0);
      test_abs_value (12, 0);
      test_abs_value (13, 0);
      test_abs_value (14, 0);
      test_abs_value (15, 0);
      conclude_test
   end;

procedure test118;
   begin
      add ('var');
      add (' r1,r2,r3,r4: real;       // 0,4,8,12');
      add (' i1,i2,i3,i4: ieee_single;     // 16,20,24,28');
      add (' r: real := 1.23;');
      add (' i: ieee_single := 1.23;const');
      add (' cr: real = 1.23;');
      add (' ci: ieee_single = 1.23;');
      add ('function rr (r: real): real;');
      add (' begin');
      add ('   result := r');
      add (' end;');
      add ('function ri (i: ieee_single): real;');
      add (' begin');
      add ('  result := i');
      add (' end;');
      add ('function ii (i: ieee_single): ieee_single;');
      add (' begin');
      add ('   result := i');
      add (' end;');
      add ('function ir (i: ieee_single): real;');
      add (' begin');
      add ('  result := i');
      add (' end;');
      add ('begin');
      add (' r1 := rr(r);');
      add (' i1 := ii(i);');
      add (' r2 := ri(i);');
      add (' i2 := ir(r);');
      add (' r3 := rr(cr);');
      add (' i3 := ii(ci);');
      add (' r4 := ri(ci);');
      add (' i4 := ir(cr);');
      add ('end.');
      start_test (118);
      test_abs_value (0, $7f, $1d, $70, $a4);
      test_abs_value (4, $7f, $1d, $70, $a4);
      test_abs_value (8, $7f, $1d, $70, $a4);
      test_abs_value (12, $7f, $1d, $70, $a4);
      test_abs_value (16, $3f, $9d, $70, $a4);
      test_abs_value (20, $3f, $9d, $70, $a4);
      test_abs_value (24, $3f, $9d, $70, $a4);
      test_abs_value (28, $3f, $9d, $70, $a4);
      conclude_test
   end;

procedure test119;
   begin
      add ('{$processor ''pic18f65j50''}');
      add ('var i0, i1: uint8;');
      add ('begin');
      add ('  refocon.roon := 1;');
      add ('  refocon.rodiv := $f;');
      add ('  i0 := refocon.roon;');
      add ('  i1 := refocon.rodiv;');
      add ('end.');
      start_test (119);
      test_sfr_value ('REFOCON', $8F);
      test_abs_value (0, 1);
      test_abs_value (1, $f);
      conclude_test
   end;

procedure test120;
   begin
      add ('type');
      add (' tr = record i,j: int16 end;');
      add (' ts = set of 0..15;');
      add ('const');
      add (' cr: tr = (i=5,j=-6);');
      add ('var');
      add (' vr0: tr;  // 0 size=4');       // 0
      add ('  i24: int24;');               // 4
      add ('  i16: -10000..-1000 := -5000;');  // 7
      add (' i9: uint8;');                     // 9
      add ('  vr10: tr;');                    // 10
      add ('  b14: boolean;');              // 14
      add ('  b15: array [9..11] of boolean;');  // 15
      add ('  s1: ts := [1,5,9]; ');          // 18
      add ('  s2: ts;');                      // 20
      add ('  err22: uint24;');               // 22
      add ('  i: int16;');                    // 25
      add ('procedure p (var p1: tr; p2: tr);');
      add ('   begin');
      add ('    p1 := p2 ');
      add ('   end;');
      add ('procedure px (var s: string);');
      add ('   var');
      add ('      i: int24;');
      add ('   begin');
      add ('      i := 300;');
      add ('      s.strlen := i + 2');
      add ('   end;');
      add ('function py (var s: string): int16;');
      add ('  begin');
      add ('    result := s.maxstrlen;');
      add ('  end;');
      add ('function msl (var s: string): uint8;');
      add (' begin');
      add ('   result := s.maxstrlen');
      add (' end;');
      add ('function f: boolean;');
      add (' begin');
      add ('   result := true');
      add (' end;');
      add ('var');
      add (' s: string[10];');
      add ('begin');
      add (' vr0 := cr;');
      add (' i24 := i16 - 1;');
      add (' i9 := msl(s);');
      add (' p (vr10, vr0);');
      add (' b14 := f;');
      add (' b15[i9] := true;');
      add (' s2 := s1;');
      add (' px(s);');
      add (' err22 := ErrorCode;');
      add (' i := py(s);');
      add ('end.');
      start_test (120);
      test_abs_value (0, 0, 5, $ff, $fa);
      test_abs_value (4, $ff, $ec, $77);
      test_abs_value (9, 10);
      test_abs_value (10, 0, 5, $ff, $fa);
      test_abs_value (14, 1);
      test_abs_value (15, 0, 1, 0);
      test_abs_value (20, $02, $22);    // s2 = [1,5,9]?
      test_run_time_error_detected (22, rterr_invalid_strlen);
      test_abs_value (25, 0, 10);
      conclude_test
   end;

procedure test121;
   begin
      add ('rom');
      add (' rr: real = 3.0;');
      add (' rra: array [0..2] of real = ([0] = 1.0, [1] = 2.0, [2] = 3.0);');
      add ('var');
      add (' i: 0..63;');                    // 0
      add (' ra: array [0..63] of real;');   // 1
      add (' r, t: real := 1.0;');           // 257, 261
      add ('begin');
      add (' ra[0] := 2.0;');
      add (' i := 0;');
      add (' r := ra[i];');
      add (' ra[i] := t;');
      add (' t := rr;');
      add (' i := 1;');
      add ('  ra[i] := rra[i];');
      add ('end.');
      start_test (121);
      test_abs_value (1, $7f, 0, 0, 0);   // ra[0] = 1.0?
      test_abs_value (257, $80, 0, 0, 0);   // r = 2.0?
      test_abs_value (261, $80, $40, 0, 0); // t = 3.0?
      test_abs_value (5, $80, 0, 0, 0);  // ra[1] = 2.0?
      conclude_test
   end;

procedure test122;    // local stack addressing, direct and indirect
   begin
      add ('{$processor ''pic18f2520''}');
      add ('function fdd: uint16;');
      add ('var x: string[89];');
      add (' begin');
      add ('   result := 7');   // fdd direct 5e,5f
      add (' end;');
      add ('function fdi: uint16;');
      add ('var x: string[90];');
      add (' begin');
      add ('   result := 8');   // fdi indirect
      add (' end;');
      add ('function fsd: uint16;');
      add (' var ');
      add ('   x: string[92];');
      add ('   i: uint16 := 5;');
      add (' begin');
      add ('   result := i');    // i direct 5e,5f
      add (' end;');
      add ('function fsi: uint16;');
      add (' var ');
      add ('   x: string[93];');
      add ('   i: uint16 := 6;');
      add (' begin');
      add ('   result := i');   // i indirect
      add (' end;');
      add ('function fmd (var s: string): uint8;');
      add ('  var x: string [93];');
      add ('  begin');
      add ('    result := s.maxstrlen');   // s.maxstrlen direct 5f
      add ('  end;');
      add ('function fmi (var s: string): uint8;');
      add ('  var x: string [94];');
      add ('  begin');
      add ('    result := s.maxstrlen');   // s.maxstrlen indirect
      add ('  end;');
      add ('function pd1 (i: int16): int16;');
      add ('  begin');
      add ('    result := i ');
      add ('  end;');
      add ('function pd2: int16;');
      add ('  var');
      add ('     x: string[87];');
      add ('     i: int16 := $77BB;');
      add ('  begin');
      add ('     result := pd1(i)');
      add ('  end;');
      add ('function pi1 (i: int16): int16;');
      add ('  begin');
      add ('    result := i ');
      add ('  end;');
      add ('function pi2: int16;');
      add ('  var');
      add ('     x: string[88];');
      add ('     i: int16 := $33AA;');
      add ('  begin');
      add ('     result := pi1(i)');
      add ('  end;');

      add ('function fff1 (var s: string): uint8;');
      add (' begin');
      add ('   result := s.maxstrlen');
      add (' end;');
      add ('function fffd (var s: string): uint8;');
      add (' var x: string[119];');
      add (' begin');
      add ('   result := fff1 (s)');
      add (' end;');
      add ('function fffi (var s: string): uint8;');
      add (' var x: string[120];');
      add (' begin');
      add ('   result := fff1 (s)');
      add (' end;');
      add ('type t =');
      add (' array [1..3] of');
      add ('    string[20];');
      add ('function gd (var tt: t): uint8;');
      add (' var i: 1..3;');
      add ('    x: string[87];');
      add (' begin');
      add ('   result := fff1(tt[i]);');
      add (' end;');
      add ('function gi (var tt: t): uint8;');
      add (' var i: 1..3;');
      add ('    x: string[88];');
      add (' begin');
      add ('   result := fff1(tt[i]);');
      add (' end;');

      add ('type tr=record i,j: int16 end;');
      add ('');
      add ('function hd (var a: tr): int16;');
      add (' var');
      add ('    x: string[88];');
      add ('    b: tr;');
      add (' begin');
      add ('   b := a;');
      add ('   result := b.i');
      add (' end;');
      add ('function hi (var a: tr): int16;');
      add (' var');
      add ('    x: string[89];');
      add ('    b: tr;');
      add (' begin');
      add ('   b := a;');
      add ('   result := b.j');
      add (' end;');

      add ('function kd (var t: tr): int16;');
      add (' var x: string [92];');
      add (' begin');
      add ('   t.j := 44;');
      add ('   result := t.j');
      add (' end;');
      add ('function ki (var t: tr): int16;');
      add (' var x: string [93];');
      add (' begin');
      add ('   t.j := 45;');
      add ('   result := t.j');
      add (' end;');


      add ('var i0,i2,i4,i6,i8,i10: uint16;');
      add ('    i12,i13: uint8;');
      add ('    i14,i16: int16;');
      add ('    i18,i19,i20,i21: uint8;');
      add ('    i22,i24,i26,i28: int16;');
      add ('var ttt: t;');
      add ('  s: string[10];');
      add ('  rr: tr := (i=123,j=66);');
      add (' c: class');
      add ('      eeprom s: string[10];');
      add ('    function fd (eeprom ss: string): uint8;');
      add ('      var ');
      add ('        x: string[93];');
      add ('      begin');
      add ('        result := ss.maxstrlen;');
      add ('      end;');
      add ('    function fi (eeprom ss: string): uint8;');
      add ('      var ');
      add ('        x: string[94];');
      add ('      begin');
      add ('        result := ss.maxstrlen;');
      add ('      end;');
      add ('    public');
      add ('    function fdd: uint8;');
      add ('      begin');
      add ('        result := fd(s) ');
      add ('      end;');
      add ('    function fii: uint8;');
      add ('      begin');
      add ('        result := fi(s) ');
      add ('      end;');
      add ('    begin');
      add ('    end;');
      add ('begin');
      add (' i0 := fdd;');
      add (' i2 := fdi;');
      add (' i4 := fsd;');
      add (' i6 := fsi;');
      add (' i8 := fmd(s);');
      add (' i10 := fmi(s);');
      add (' i12 := c.fdd;');
      add (' i13 := c.fii;');
      add (' i14 := pd2;');
      add (' i16 := pi2;');
      add (' i18 := fffd(s);');
      add (' i19 := fffi(s);');
      add (' i20 := gd(ttt);');
      add (' i21 := gi(ttt);');
      add (' i22 := hd(rr);');
      add (' i24 := hi(rr);');
      add (' i26 := kd(rr);');
      add (' i28 := ki(rr);');
      add ('end.');
      start_test (122);
      test_abs_value (0, 0, 7);
      test_abs_value (2, 0, 8);
      test_abs_value (4, 0, 5);
      test_abs_value (6, 0, 6);
      test_abs_value (8, 0, 10);
      test_abs_value (10, 0, 10);
      test_abs_value (12, 10);
      test_abs_value (13, 10);
      test_abs_value (14, $77, $BB);
      test_abs_value (16, $33, $AA);
      test_abs_value (18, 10);
      test_abs_value (19, 10);
      test_abs_value (20, 20);
      test_abs_value (21, 20);
      test_abs_value (22, 0, 123);
      test_abs_value (24, 0, 66);
      test_abs_value (26, 0, 44);
      test_abs_value (28, 0, 45);
      conclude_test
   end;

procedure test123;
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add (' tc=class');
      add ('      eeprom i: int16;');
      add ('      function f (eeprom ii: int16): int16;');
      add ('        begin');
      add ('           result := ii ');
      add ('        end;');
      add ('      function f1 (eeprom ii: int16): int16;');
      add ('        var x: string[88];');
      add ('        begin');
      add ('           result := f(ii);  ');
      add ('        end;');
      add ('      function f2 (eeprom ii: int16): int16;');
      add ('        var x: string[89];');
      add ('        begin');
      add ('           result := f(ii);  ');
      add ('        end;');
      add ('    public');
      add ('      function ff1: int16;');
      add ('        begin');
      add ('          i := $123;');
      add ('          result := f1(i)');
      add ('        end;');
      add ('      function ff2: int16;');
      add ('        begin');
      add ('          i := $456;');
      add ('          result := f2(i)');
      add ('        end; ');
      add ('      begin ');
      add ('      end;');
      add ('var');
      add ('  i0,i2: int16;');
      add ('  x,c: tc;');
      add ('begin');
      add ('  i0 := c.ff1;');
      add ('  i2 := c.ff2; ');
      add ('end.');
      start_test (123);
      test_abs_value (0, 1, $23);
      test_abs_value (2, 4, $56);
      conclude_test
   end;

procedure test124;
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add (' tr=record a,b,c: int16 end;');
      add (' tc=class');
      add ('      eeprom i: tr;');
      add ('      function f (eeprom ii: int16): int16;');
      add ('        begin');
      add ('           result := ii');
      add ('        end;');
      add ('      function f1 (eeprom ii: tr): int16;');
      add ('        var x: string[88];');
      add ('        begin');
      add ('           result := f(ii.b)');
      add ('        end;');
      add ('      function f2 (eeprom ii: tr): int16;');
      add ('        var x: string[89];');
      add ('        begin');
      add ('           result := f(ii.b)');
      add ('        end;');
      add ('     public');
      add ('      function ff1: int16;');
      add ('        begin');
      add ('          i.b := $123;');
      add ('          result := f1(i)');
      add ('        end;');
      add ('      function ff2: int16;');
      add ('        begin');
      add ('          i.b := $456;');
      add ('          result := f2(i)');
      add ('        end;');
      add ('      begin');
      add ('      end;');
      add ('var');
      add ('  i0,i2: int16;');
      add ('  x,c: tc;');
      add ('begin');
      add ('  i0 := c.ff1;');
      add ('  i2 := c.ff2;');
      add ('end.');
      start_test (124);
      test_abs_value (0, 1, $23);
      test_abs_value (2, 4, $56);
      conclude_test
   end;

procedure test125;
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type tc=class');
      add ('   eeprom');
      add ('      x: int16; ');
      add ('      ss: string[1];');
      add ('   procedure pnear (eeprom s: string);');
      add ('      var ');
      add ('        c: char := ''a'';');
      add ('        x: string[90];');
      add ('      begin ');
      add ('         s := c ');
      add ('      end;');
      add ('   procedure pfar (eeprom s: string);');
      add ('      var ');
      add ('        c: char := ''b'';');
      add ('        x: string[91];');
      add ('      begin ');
      add ('         s := c ');
      add ('      end;');
      add ('    public');
      add ('  function f1: char;');
      add ('     begin');
      add ('        pnear(ss);');
      add ('        result := ss[1]');
      add ('     end;');
      add ('  function f2: char;');
      add ('     begin');
      add ('        pfar(ss);');
      add ('        result := ss[1]');
      add ('     end;');
      add ('  begin ');
      add ('  end;');
      add ('var');
      add ('  c0,c1: char;');
      add ('  c: tc;');
      add ('begin ');
      add ('  c0 := c.f1;');
      add ('  c1 := c.f2;');
      add ('end.');
      start_test (125);
      test_abs_value (0, ord('a'));
      test_abs_value (1, ord('b'));
      conclude_test
   end;

procedure test126;
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add (' tx = record a,b,c: char end; ');
      add (' tc=class');
      add ('   eeprom');
      add ('      x: int16; ');
      add ('      ss: tx;');
      add ('   procedure pnear (eeprom s: tx);');
      add ('      var ');
      add ('        c: char := ''a'';');
      add ('        x: string[92];');
      add ('      begin ');
      add ('         s.b := c ');
      add ('      end;');
      add ('   procedure pfar (eeprom s: tx);');
      add ('      var ');
      add ('        c: char := ''b'';');
      add ('        x: string[93];');
      add ('      begin ');
      add ('         s.b := c ');
      add ('      end;');
      add ('   public');
      add ('  function f1: char;');
      add ('     begin');
      add ('        pnear(ss);');
      add ('        result := ss.b');
      add ('     end;');
      add ('  function f2: char;');
      add ('     begin');
      add ('        pfar(ss);');
      add ('        result := ss.b');
      add ('     end;');
      add ('  begin ');
      add ('  end;');
      add ('var');
      add ('  c0,c1: char;');
      add ('  c: tc;');
      add ('begin');
      add ('  c0 := c.f1;');
      add ('  c1 := c.f2; ');
      add ('end.');
      start_test (126);
      test_abs_value (0, ord('a'));
      test_abs_value (1, ord('b'));
      conclude_test
   end;

procedure test127;    // test "simple" index calculations
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add ('  tc=class');
      add ('        eeprom');
      add ('          arr8: array [uint3] of uint8;');
      add ('          arr16: array [uint3] of uint16;');
      add ('        procedure init_eeprom;');
      add ('          var');
      add ('            i: 0..10;');
      add ('          begin');
      add ('             for i := 0 to 7 do');
      add ('               begin');
      add ('                 arr8[i] := 100-i;');
      add ('                 arr16[i] := 1000-i');
      add ('               end');
      add ('          end;');
      add ('      public');
      add ('        function f8 (idx: uint3): uint8;');
      add ('          begin');
      add ('            result := arr8[idx]');
      add ('          end;');
      add ('        function f16 (idx: uint3): uint16;');
      add ('          begin');
      add ('            result := arr16[idx]');
      add ('          end;');
      add ('       begin');
      add ('          init_eeprom');
      add ('       end;');
      add ('var');
      add (' i0: 1..255;');
      add (' i1: uint8;');
      add (' i2: uint16;');
      add (' c: tc; ');
      add (' idx: 0..300;');
      add (' arr: array [0..300] of uint8;');
      add ('begin');
      add (' init c;');
      add (' for i0 := 1 to 255');
      add (' do arr[i0-1] := i0;');
      add (' idx := 123;');
      add (' i0 := arr[idx];');
      add (' i1 := c.f8 (3);');
      add (' i2 := c.f16 (5);');
      add ('end.');
      start_test (127);
      test_abs_value (0, 124);
      test_abs_value (1, 100-3);
      test_abs_value (2, msb(1000-5), lsb(1000-5));
      conclude_test
   end;

procedure test128;    // test with eeprom
   begin
      add ('{$processor ''pic18f2520''}');
      add ('type');
      add (' tc=class');
      add ('      eeprom');
      add ('        xx: int24;');
      add ('        x: record i,j: int16 end := (i=$123, j=$456);');
      add ('        r: record i,j: int16 end := (i=$345, j=$678);');
      add ('      function f (eeprom ii: int16): int16;');
      add ('        begin');
      add ('          result := ii');
      add ('        end;');
      add ('    public');
      add ('      function ff: int16;');
      add ('         begin');
      add ('           with x do result := f(j);  ');
      add ('         end;');
      add ('      function fi: int16;');
      add ('        begin');
      add ('          with r do');
      add ('            result := i');
      add ('        end;');
      add ('      function fj: int16;');
      add ('        begin');
      add ('          with r do');
      add ('            result := j');
      add ('        end;');
      add ('      begin');
      add ('      end;');
      add ('var');
      add ('   i0,i2,i4: int16;');
      add ('   c: tc;');
      add ('begin');
      add ('   i0 := c.ff;');
      add ('   i2 := c.fi;');
      add ('   i4 := c.fj;');
      add ('end.');
      start_test (128);
      test_abs_value (0, $4, $56);
      test_abs_value (2, $3, $45);
      test_abs_value (4, $6, $78);
      conclude_test
   end;

procedure test129;    // test with ram
   begin
      add ('var');
      add ('  i0,i2: int16;');
      add ('  r: record i,j: int16 end := (i=$123, j=$456);');
      add ('function f (var ii: int16): int16;');
      add ('  begin');
      add ('    result := ii');
      add ('  end;');
      add ('begin');
      add ('  with r do');
      add ('     begin');
      add ('        i0 := f(i);');
      add ('        i2 := f(j)');
      add ('     end');
      add ('end.');
      start_test (129);
      test_abs_value (0, $1, $23);
      test_abs_value (2, $4, $56);
      conclude_test
   end;

procedure test130;    // test add index offset
   begin
      add ('type');
      add ('  tneg = -3..-1;');
      add ('function f (var i: uint8): uint8;');
      add ('  begin');
      add ('    result := i');
      add ('  end;');
      add ('function fa (var idx: uint24): uint8;');
      add ('  var');
      add ('    arr1: array [uint2] of uint8;');
      add ('    i: uint2;');
      add ('  begin');
      add ('    for i := 0 to 3');
      add ('    do arr1[i] := 20+i;');
      add ('    result := f(arr1[idx])');
      add ('  end;');
      add ('function fb (var idx: int2): uint8;');
      add ('  var');
      add ('    arr1: array [int2] of uint8;');
      add ('    i: int2;');
      add ('  begin');
      add ('    for i := -2 to 1');
      add ('    do arr1[i] := 40+i;');
      add ('    result := f(arr1[idx])');
      add ('  end;');
      add ('function fc (var idx: tneg): uint8;');
      add ('  var');
      add ('    arr1: array [tneg] of uint8;');
      add ('    i: tneg;');
      add ('  begin');
      add ('    for i := -3 to -1');
      add ('    do arr1[i] := 60+i;');
      add ('    result := f(arr1[idx])');
      add ('  end;');
      add ('function fd (var idx: uint2): uint8;');
      add ('  var');
      add ('    arr1: array [uint2] of uint8;');
      add ('    i: uint2;');
      add ('  begin');
      add ('    for i := 0 to 3');
      add ('    do arr1[i] := 80+i;');
      add ('    result := f(arr1[idx])');
      add ('  end;');
      add ('function f16 (var i: uint16): uint16;');
      add ('  begin');
      add ('    result := i');
      add ('  end;');
      add ('function fa16 (var idx: uint24): uint16;');
      add ('  var');
      add ('    arr1: array [uint2] of uint16;');
      add ('    i: uint2;');
      add ('  begin');
      add ('    for i := 0 to 3');
      add ('    do arr1[i] := 20+i;');
      add ('    result := f16(arr1[idx])');
      add ('  end;');
      add ('function fb16 (var idx: int2): uint16;');
      add ('  var');
      add ('    arr1: array [int2] of uint16;');
      add ('    i: int2;');
      add ('  begin');
      add ('    for i := -2 to 1');
      add ('    do arr1[i] := 40+i;');
      add ('    result := f16(arr1[idx])');
      add ('  end;');
      add ('function fc16 (var idx: tneg): uint16;');
      add ('  var');
      add ('    arr1: array [tneg] of uint16;');
      add ('    i: tneg;');
      add ('  begin');
      add ('    for i := -3 to -1');
      add ('    do arr1[i] := 60+i;');
      add ('    result := f16(arr1[idx])');
      add ('  end;');
      add ('function fd16 (var idx: uint2): uint16;');
      add ('  var');
      add ('    arr1: array [uint2] of uint16;');
      add ('    i: uint2;');
      add ('  begin');
      add ('    for i := 0 to 3');
      add ('    do arr1[i] := 80+i;');
      add ('    result := f16(arr1[idx])');
      add ('  end;');
      add ('var');
      add ('  i0,i1,i2,i3: uint8;');
      add ('  i4,i6,i8,i10: uint16; ');
      add ('  i12: uint8; i13: uint16;');
      add ('  ix: uint8;');
      add ('  idxa: uint24 := 2;');
      add ('  idxb: int2 := -1;');
      add ('  idxb2: int2 := 1;');
      add ('  idxc: tneg := -2;');
      add ('  idxd: uint2 := 3;');
      add ('begin');
      add ('  i0 := fa(idxa);');
      add ('  i1 := fb(idxb);');
      add ('  i2 := fc(idxc);');
      add ('  i3 := fd(idxd);');
      add ('  i4 := fa16(idxa);');
      add ('  i6 := fb16(idxb);');
      add ('  i8 := fc16(idxc);');
      add ('  i10 := fd16(idxd);');
      add ('  i12 := fb(idxb2);');
      add ('  i13 := fb16(idxb2);');
      add ('end.');
      start_test (130);
      test_abs_value (0, 22);
      test_abs_value (1, 39);
      test_abs_value (2, 58);
      test_abs_value (3, 83);
      test_abs_value (4, 0, 22);
      test_abs_value (6, 0, 39);
      test_abs_value (8, 0, 58);
      test_abs_value (10, 0, 83);
      test_abs_value (12, 41);
      test_abs_value (13, 0, 41);
      conclude_test
   end;

procedure test131;    // for loop with constants
   begin
      add ('function f_incrc_near1: uint8;');
      add ('  var');
      add ('    j: uint8;');
      add ('    x: string[92];');
      add ('    i: uint8;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 4 to $ff do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_incrc_far1: uint8;');
      add ('  var');
      add ('    j: uint8;');
      add ('    x: string[93];');
      add ('    i: uint8;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 2 to $ff do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_incrc_near2: uint16;');
      add ('  var');
      add ('    j: uint16;');
      add ('    x: string[90];');
      add ('    i: uint16;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 1 to 5 do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_incrc_far2: uint16;');
      add ('  var');
      add ('    j: uint16;');
      add ('    x: string[92];');
      add ('    i: uint16;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 3 to $100 do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_decc_near1: uint8;');
      add ('  var');
      add ('    j: uint8;');
      add ('    x: string[92];');
      add ('    i: uint8;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 200 downto 1 do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_decc_far1: uint8;');
      add ('  var');
      add ('    j: uint8;');
      add ('    x: string[93];');
      add ('    i: uint8;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 101 downto 0 do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_decc_near2: uint16;');
      add ('  var');
      add ('    j: uint16;');
      add ('    x: string[90];');
      add ('    i: uint16;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 222 downto 200 do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('function f_decc_far2: uint16;');
      add ('  var');
      add ('    j: uint16;');
      add ('    x: string[92];');
      add ('    i: uint16;');
      add ('  begin');
      add ('    j := 0;');
      add ('    for i := 33 downto 7 do');
      add ('      j := j + 1;');
      add ('    result := j');
      add ('  end;');
      add ('var');
      add ('  i0,i1: uint8;');
      add ('  i2,i4: uint16;');
      add ('  i6,i7: uint8;');
      add ('  i8,i10: uint16;');
      add ('  i12: uint8;');
      add ('  i13: uint16;');
      add ('  i15: uint8;');
      add ('  i16: uint16;');
      add ('  i18: uint8;');
      add ('  i19: uint16;');
      add ('  i21: uint16;');
      add ('  i23: uint8;');
      add ('  in8: uint8;');
      add ('  in16: uint16;');
      add ('  x: string[255];');
      add ('  if8: uint8;');
      add ('  if16: uint16;');
      add (' c: class');
      add ('      var i8: uint8;');
      add ('          i16: uint16;');
      add ('          i: uint8;');
      add ('          j: uint16;');
      add ('    public');
      add ('      function f8: uint8;');
      add ('        begin result := i8 end;');
      add ('      function f16: uint16;');
      add ('        begin result := i16 end;');
      add ('      begin');
      add ('        for i := 7 downto 3');
      add ('        do i8 := i8 + 1;');
      add ('        for j := 700 downto $FF');
      add ('        do i16 := i16 + 1');
      add ('      end;');
      add ('begin');
      add ('  init c;');
      add ('  i0 := f_incrc_near1;');
      add ('  i1 := f_incrc_far1;');
      add ('  i2 := f_incrc_near2;');
      add ('  i4 := f_incrc_far2;');
      add ('  i6 := f_decc_near1;');
      add ('  i7 := f_decc_far1;');
      add ('  i8 := f_decc_near2;');
      add ('  i10 := f_decc_far2;');
      add ('  for in8 := 10 to $ff do');
      add ('     i12 := i12 + 1;');
      add ('  for in16 := 100 to $100 do');
      add ('     i13 := i13 + 1;');
      add ('  for if8 := 100 to $FF do');
      add ('     i15 := i15 + 1;');
      add ('  for if16 := 1000 to 3000 do');
      add ('     i16 := i16 + 1;');
      add ('  i18 := c.f8;');
      add ('  i19 := c.f16;');
      add ('  for in16 := 1000 downto $100 do');
      add ('    i21 := i21 + 1;');
      add ('  for in8 := 2 to 5 do');
      add ('    i23 := i23 + 1;');
      add ('end.');
      start_test (131);
      test_abs_value (0, 252);
      test_abs_value (1, 254);
      test_abs_value (2, 0, 5);
      test_abs_value (4, 0, 254);
      test_abs_value (6, 200);
      test_abs_value (7, 102);
      test_abs_value (8, 0, 23);
      test_abs_value (10, 0, 27);
      test_abs_value (12, 246);
      test_abs_value (13, 0, 157);
      test_abs_value (15, 156);
      test_abs_value (16, msb(2001), lsb(2001));
      test_abs_value (18, 5);
      test_abs_value (19, msb(446), lsb(446));
      test_abs_value (21, msb(745), lsb(745));
      test_abs_value (23, 4);
      conclude_test
   end;

procedure test132;    // for loop with vars
   begin
      add ('function fn: uint16;');
      add (' var');
      add ('   l: uint16;');
      add ('   x: string[84];');
      add ('   i: uint16;');
      add (' begin');
      add ('   l := 1004;');
      add ('   for i := 108 to l do');
      add ('     result := result + 1');
      add ('   end;');
      add ('function ff: uint16;');
      add (' var');
      add ('   l: uint16;');
      add ('   x: string[85];');
      add ('   i: uint16;');
      add (' begin');
      add ('   l := 1234;');
      add ('   for i := 544 to l do');
      add ('     result := result + 1');
      add ('   end;');
      add ('type');
      add ('  tfv8 = 200..255;');
      add ('  tfv16 = 300..400;');
      add ('var');
      add ('  i0: uint8;');
      add ('  i1: uint16;');
      add ('  i3: uint8;');
      add ('  i4: uint16;');
      add ('  i6: uint16;');
      add ('  i8: uint8;');
      add ('  i9: uint16;');
      add ('  i11: uint16;');
      add ('');
      add ('  fv8: tfv8;');
      add ('  fv16: tfv16;');
      add ('  ii: uint16;');
      add ('  x: string[255];');
      add ('  if8: uint8;');
      add ('  if16: uint16;');
      add ('  li,lf: 10..20;');
      add ('begin');
      add ('  fv8 := 220;');
      add ('  for if8 := 10 to fv8 do');
      add ('    i0 := i0 + 1;');
      add ('  fv16 := 350;');
      add ('  for if16 := 10 to fv16 do');
      add ('    i1 := i1 + 1;');
      add ('  ii := 220;');
      add ('  for if8 := 10 to ii do');
      add ('    i3 := i3 + 1;');
      add ('  ii := 350;');
      add ('  for if16 := 10 to ii do');
      add ('    i4 := i4 + 1;');
      add ('  for if16 := i4 to 400 do');
      add ('    i6 := i6 + 1;');
      add ('  li := 18;');
      add ('  lf := 12;');
      add ('  for if8 := li downto lf do');
      add ('    i8 := i8 + 1;');
      add ('  i9 := fn;');
      add ('  i11 := ff;');
      add ('end.');
      start_test (132);
      test_abs_value (0, 211);
      test_abs_value (1, msb(350-10+1), lsb(350-10+1));
      test_abs_value (3, 211);
      test_abs_value (4, msb(350-10+1), lsb(350-10+1));
      test_abs_value (6, 0, 60);
      test_abs_value (8, 7);
      test_abs_value (9, msb(1004-108+1), lsb(1004-108+1));
      test_abs_value (11, msb(1234-544+1), lsb(1234-544+1));
      conclude_test
   end;

procedure test133;    // test undimensioned string indexing
   begin
      add ('function fu8n (var s: string; i: uint8): uint24;');
      add (' var');
      add ('  c: char;');
      add ('  x: string[88];');
      add (' begin');
      add ('  c := s[i];');
      add ('  result := ErrorCode');
      add (' end;');
      add ('function fu8f (var s: string; i: uint8): uint24;');
      add (' var');
      add ('  c: char;');
      add ('  x: string[89];');
      add (' begin');
      add ('  c := s[i];');
      add ('  result := ErrorCode');
      add (' end;');
      add ('function fs8 (var s: string; i: int8): uint24;');
      add (' var');
      add ('  c: char;');
      add (' begin');
      add ('  c := s[i];');
      add ('  result := ErrorCode');
      add (' end;');
      add ('function fs24 (var s: string; i: int24): uint24;');
      add (' var');
      add ('  c: char;');
      add (' begin');
      add ('  c := s[i];');
      add ('  result := ErrorCode');
      add (' end;');
      add ('function fu8nc (s: string; i: uint8): uint24;');
      add (' var');
      add ('  c: char;');
      add ('  x: string[87];');
      add (' begin');
      add ('  c := s[i];');
      add ('  result := ErrorCode');
      add (' end;');
      add ('function fu8fc (s: string; i: uint8): uint24;');
      add (' var');
      add ('  c: char;');
      add ('  x: string[88];');
      add (' begin');
      add ('  c := s[i];');
      add ('  result := ErrorCode');
      add (' end;');
      add ('');
      add ('var');
      add (' err0,err3,err6,err9,err12,err15: uint24;');
      add (' err18,err21: uint24;');
      add (' err24,err27,err30,err33,err36: uint24;');
      add (' err39,err42,err45,err48,err51,err54: uint24;');
      add (' err57,err60,err63,err66: uint24;');
      add (' err69,err72: uint24;');
      add ('');
      add (' s10: string[10] := ''12345678'';');
      add (' s100: string[100];');
      add (' s200: string[200];');
      add ('');
      add (' c: class (s: string; i: int16);');
      add ('       var c: char;');
      add ('    public');
      add ('      procedure p; begin end;');
      add ('       begin');
      add ('         c := s[i]');
      add ('       end;');
      add ('');
      add ('begin');
      add (' err0 := fu8n (s10, 0);');
      add (' err3 := fu8n (s10, 1);');
      add (' err6 := fu8n (s10, 10);');
      add (' err9 := fu8n (s10, 11);');
      add (' err12 := fu8n (s200, 200);');
      add (' err15 := fu8n (s200, 201);');
      add ('');
      add (' err18 := fs8 (s10, -1);');
      add (' err21 := fs8 (s100, 101);');
      add ('');
      add (' err24 := fs24(s10, -8388607);');
      add (' err27 := fs24(s10, $010001);');
      add (' err30 := fs24(s10, $000101);');
      add (' err33 := fs24(s10, 1);');
      add (' err36 := fs24(s100, 0);');
      add ('');
      add (' err39 := fu8f (s10, 0);');
      add (' err42 := fu8f (s10, 1);');
      add (' err45 := fu8f (s10, 10);');
      add (' err48 := fu8f (s10, 11);');
      add (' err51 := fu8f (s200, 200);');
      add (' err54 := fu8f (s200, 201);');
      add ('');
      add (' err57 := fu8nc (s10, 8);');
      add (' err60 := fu8nc (s10, 9);');
      add (' err63 := fu8fc (s10, 8);');
      add (' err66 := fu8fc (s10, 9);');
      add ('');
      add (' init c (s10, 8);');
      add (' err69 := ErrorCode;');
      add (' init c (s10, 9);');
      add (' err72 := ErrorCode;');
      add ('');
      add ('end.');

      start_test (133);
      test_run_time_error_detected (9, rterr_out_of_bounds_array_index);
      test_abs_value (3, 0, 0, 0);
      test_abs_value (6, 0, 0, 0);
      test_run_time_error_detected (9, rterr_out_of_bounds_array_index);
      test_abs_value (12, 0, 0, 0);
      test_run_time_error_detected (15, rterr_out_of_bounds_array_index);

      test_run_time_error_detected (18, rterr_out_of_bounds_array_index);
      test_run_time_error_detected (21, rterr_out_of_bounds_array_index);

      test_run_time_error_detected (24, rterr_out_of_bounds_array_index);
      test_run_time_error_detected (27, rterr_out_of_bounds_array_index);
      test_run_time_error_detected (30, rterr_out_of_bounds_array_index);
      test_abs_value (33, 0, 0, 0);
      test_run_time_error_detected (36, rterr_out_of_bounds_array_index);

      test_run_time_error_detected (39, rterr_out_of_bounds_array_index);
      test_abs_value (42, 0, 0, 0);
      test_abs_value (45, 0, 0, 0);
      test_run_time_error_detected (48, rterr_out_of_bounds_array_index);
      test_abs_value (51, 0, 0, 0);
      test_run_time_error_detected (54, rterr_out_of_bounds_array_index);

      test_abs_value (57, 0, 0, 0);
      test_run_time_error_detected (60, rterr_out_of_bounds_array_index);
      test_abs_value (63, 0, 0, 0);
      test_run_time_error_detected (66, rterr_out_of_bounds_array_index);

      test_abs_value (69, 0, 0, 0);
      test_run_time_error_detected (72, rterr_out_of_bounds_array_index);
      conclude_test
   end;

procedure test134;
   begin
      add ('function fn (var s: string): uint8;');
      add (' var x: string[92];');
      add (' begin');
      add ('  s.strlen := 15;');
      add ('  result := s.strlen');
      add (' end;');
      add ('function ff (var s: string): uint8;');
      add (' var x: string[93];');
      add (' begin');
      add ('  s.strlen := 44;');
      add ('  result := s.strlen');
      add (' end;');
      add ('var');
      add (' i0,i1: uint8; ');
      add (' s: string[100];');
      add ('begin');
      add (' i0 := fn(s);');
      add (' i1 := ff(s);');
      add ('end.');
      start_test (134);
      test_abs_value (0, 15);
      test_abs_value (1, 44);
      conclude_test
   end;

procedure test135;   // test includes
   begin
      add ('type');
      add ('{$include ''include_test1\c2.inc''}');
      add ('{$include ''include_test1\c3.inc''}');
      add ('var');
      add (' c1: tc1;');
      add (' c2: tc2;');
      add (' c3: tc3;');
      add ('begin');
      add (' init c1, c2, c3');
      add ('end.');
      start_test (135);
      conclude_test
   end;

procedure RunTests;
   begin
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test8;
      test9;
      test10;
      test11;
      test12;
      test13;
      test14;
      test15;
      test16;
      test17;
      test18;
      test19;
      test20;
      test21;
      test22;
      test23;
      test24;
      test25;
      test26;
      test27;
      test28;
      test29;
      test30;
      test31;
      test32;
      test33;
      test34;
      test35;
      test36;
      test37;
      test38;
      test39;
      test40;
      test41;
      test42;
      test43;
      test44;
      test45;
      test46;
      test47;
      test48;
      test49;
      test50;
      test51;
      test52;
      test53;
      test54;
      test55;
      test56;
      test57;
      test58;
      test59;
      test60;
      test61;
      test62;
      test63;
      test64;
      test65;
      test66;
      test67;
      test68;
      test69;
      test70;
      test71;
      test72;
      test73;
      test74;
      test75;
      test76;
      test77;
      test78;
      test79;
      test80;
      test81;
      test82;
      test83;
      test84;
      test85;
      test86;
      test87;
      test88;
      test89;
      test90;
      test91;
      test92;
      test93;
      test94;
      test95;
      test96;
      test97;
      test98;
      test99;
      test100;
      test101;
      test102;
      test103;
      test104;
      test105;
      test106;
      test107;
      test108;
      test109;
      test110;
      test111;
      test112;
      test113;
      test114;
      test115;
      test116;
      test117;
      test118;
      test119;
      test120;
      test121;
      test122;
      test123;
      test124;
      test125;
      test126;
      test127;
      test128;
      test129;
      test130;
      test131;
      test132;
      test133;
      test134;
      test135;
      MainForm.TestResultsMemo.Lines.Add ('all tests done')
   end;

END.
