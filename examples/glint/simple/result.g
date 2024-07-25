result_t : sum {
  error :[byte view];
  data :[byte view];
};

foo :result_t() {
  return !{ .error "" }
}
