exception Overflow;

let (+) = (a: int, b: int): int => {
  let c = a + b;
  if (a lxor b lor (a lxor lnot(c)) < 0) {
    c;
  } else {
    raise(Overflow);
  };
};

let (-) = (a: int, b: int): int => {
  let c = a - b;
  if (a lxor lnot(b) lor (b lxor c) < 0) {
    c;
  } else {
    raise(Overflow);
  };
};

let ( * ) = (a: int, b: int): int => {
  let c = a * b;
  if (Int64.of_int(c) == Int64.mul(Int64.of_int(a), Int64.of_int(b))) {
    c;
  } else {
    raise(Overflow);
  };
};

let (/) = (a: int, b: int): int =>
  if (a == min_int && b == (-1)) {
    raise(Overflow);
  } else {
    a / b;
  };

let (~-) = (x: int): int =>
  if (x != min_int) {
    - x;
  } else {
    raise(Overflow);
  };

/* take in a string, s, and print it with green color */
let print_green = (s: string): unit =>
  print_string("\027[32m" ++ s ++ "\027[0m\n");

/* take in a string, s, and print it with red color */
let print_red = (s: string): unit =>
  print_string("\027[31m" ++ s ++ "\027[0m\n");

type result('a) =
  | Actual_Result('a)
  | Expected_Result('a)
  | Actual_Error(string)
  | Expected_Error(string);

type check_result('a) =
  | Test_Passed
  | Test_Failed(result('a), result('a));

/* check-expect
  Inputs: actual and expected, two 'a and message, a string
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
  let check_expect = (actual: 'a, expected: 'a, message: string): check_result('a) =>
  if (actual == expected) {
    print_green("ce_Success: " ++ message);
    Test_Passed;
  } else {
    print_red("ce_Fail: " ++ message);
    print_red("expected output: ");
    Js.log(expected);
    print_red("actual output: ");
    Js.log(actual)
    Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

/* check-expect_list_alpha
  Inputs: actual and expected, two list('a) and message, a string
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let check_expect_list_alpha = (actual: 'a, expected: 'a, message: string): check_result('a) =>
  if (actual == expected) {
    print_green("ce_Success: " ++ message);
    Test_Passed;
  } else {
    print_red("ce_Fail: " ++ message);
    print_red("expected output: ");
    Js.log(Array.of_list(expected));
    print_red("actual output: ");
    Js.log(Array.of_list(actual));
    Test_Failed(Actual_Result(actual), Expected_Result(expected));
  };

  /* check-expect_list_list_alpha
  Inputs: actual and expected, two list(list('a)) and message, a string
  Output: Test_Passed if actual and expected are equal, or Test_Failed otherwise */
let check_expect_list_list_alpha = (actual: 'a, expected: 'a, message: string): check_result('a) =>
if (actual == expected) {
  print_green("ce_Success: " ++ message);
  Test_Passed;
} else {
  print_red("ce_Fail: " ++ message);
  print_red("expected output: ");
  Js.log(Array.of_list(List.map(((x) => Array.of_list(x)), expected)));
  print_red("actual output: ");
  Js.log(Array.of_list(List.map(((x) => Array.of_list(x)), actual)));
  Test_Failed(Actual_Result(actual), Expected_Result(expected));
};

/* check_within
  Input: Three floats, input, expected, and within. Input is a given value to be checked.
  Output: The boolean "true" if input lies within the range (expected - within) and (expected + within),
   		and "false" otherwise. */
let check_within =
    (input: float, expected: float, within: float): check_result(float) =>
  if (abs_float(input -. expected) <= abs_float(within)) {
    print_green("cw_Success ");
    Test_Passed;
  } else {
    print_red("cw_Fail ");
    Test_Failed(Actual_Result(input), Expected_Result(expected));
  };

/* check_error
  Input: a one-argument procedure 'thunk' that returns the thing you want to test when it's applied to an int
          and a string of the error message of the 'failwith' clause in the procedure
   Output: a Test_Passed or Test_Failed */
let check_error = (input: unit => 'a, expect: string): check_result('a) =>
  /* ignore(input()); */
  try (
    {
      Test_Failed(Actual_Result(input()), Expected_Error(expect));
    }
  ) {
  | Failure(err) when err == expect =>
    print_green("ce_Success ");
    Test_Passed;
  | Failure(err) =>
    print_red("err_Fail ");
    Test_Failed(Actual_Error(err), Expected_Error(expect));
  };