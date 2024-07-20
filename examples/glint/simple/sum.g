foo : sum { x :cint 0, y :uint 0 };
bar :foo;
bar.x := 69;

out :uint 0;

if (has bar.x)
  out := bar.x;
else if (has bar.y)
  out := bar.y;

out;
