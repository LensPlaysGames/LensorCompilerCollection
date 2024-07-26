external puts :cint(:byte.ptr) discardable;

print :: fmap {
  s :[byte] format argument;
  puts s.data;
  -s;
};

format :[byte](s:int) {
  negative :bool s < 0;

  ;; ensure number is positive
  if negative, s := -s;

  out :[byte];

  ;; Idea for for loop without increment (just for "program matching control flow" sake)
  ;;
  ;; initwhile
  ;;   i :: 0;
  ;;   s  > 0; {
  ;;     ;; ~= push to beginning
  ;;     ;; += push to end
  ;;     ;; b"0" -> byte value (like character literal but for a textual byte). NOT
  ;;     ;; a string in any means, `b"` is it's own token.
  ;;     out ~= (s % 10) + b"0";
  ;;     s /= 10;
  ;;     i += 1;
  ;; };
  ;;
  ;; equivalent to following current valid syntax
  ;;
  ;; {
  ;;   i :: 0;
  ;;   while s > 0 {
  ;;     ;; ~= push to beginning
  ;;     ;; += push to end
  ;;     ;; b"0" -> byte value (like character literal but for a textual byte). NOT
  ;;     ;; a string in any means, `b"` is it's own token.
  ;;     out ~= (s % 10) + b"0";
  ;;     s /= 10;
  ;;     i += 1;
  ;;   };
  ;; };

  cfor;
    i :: 0;
    s  > 0;
    {s /= 10; i += 1;}; {
    ;; ~= push to beginning
    ;; += push to end
    ;; b"0" -> byte value (like character literal but for a textual byte). NOT
    ;; a string in any means, `b"` is it's own token.
    out ~= (s % 10) + b"0";
  };

  if negative out ~= b"-";

  ;; null terminate it.
  out += b"\0";

  return out;
};

format :[byte](s:[byte view]) s;

print 69;
