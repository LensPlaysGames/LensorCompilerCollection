external puts :cint(:byte.ptr) discardable;

format :[byte](s:int) {
  negative :bool s < 0;

  ;; ensure number is positive
  if negative, s := -s;

  out :[byte];

  cfor;
    i :: 0;
    s  > 0;
    {s := s / 10; i := i + 1;}; {
    ;; ~= push to beginning
    ;; += push to end
    ;; b"0" -> byte value (like character literal but for a textual byte). NOT
    ;; a string in any means, `b"` is it's own token.
    out ~= (s % 10) + 48;
  };

  if negative, out ~= b"-";

  ;; null terminate it.
  out += 0;

  return out;
};

format :[byte](s:[byte view]) s;

print :void(i:int) {
  s :: format i;
  puts s.data;
};

print 69;
