package Msort is
	LENGTH : Integer := 40;
	type Arr is array(1 .. LENGTH) of Integer range -300 .. 300;
	procedure Sort(A : in out Arr);
end Msort;
