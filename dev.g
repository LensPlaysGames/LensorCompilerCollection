format : [byte](c : byte) {
  out : [byte];
  out += c;
  out += `\0`;
  return out;
};

foo : [byte];
foo += `4`;
foo += `2`;
foo += `\0`;

foo[2] += `0`;

for c in foo, {
  print c;
};

print foo;

-foo;
