foo : [byte];
;; append
foo += `2`;
;; prepend
foo ~= `4`;
;; append
foo += `\0`;

;; insert
foo[2] += `0`;

;; ranged for
for c in foo, {
  print c;
  print `\n`;
};

cfor
    i :: 0;
    i < foo.size;
    i += 1;
  print @foo[i];

;; print
print foo;
print `\n`;

-foo;
