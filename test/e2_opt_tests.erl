-module(e2_opt_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Opts = e2_opt:validate([{color, "blue"}], [color]),
    ?assertEqual("blue", e2_opt:value(color, Opts)).

required_test() ->
    ?assertError({required, name}, e2_opt:validate([], [name])).

default_test() ->
    Schema = [{name, [{default, "Sam"}]}],
    ?assertEqual("Sam", e2_opt:value(name, e2_opt:validate([], Schema))),
    ?assertEqual("Bob", e2_opt:value(name, e2_opt:validate(
                                             [{name, "Bob"}], Schema))).

undefined_default_val_test() ->
    Opts = e2_opt:validate([], [{name, [{default, undefined}]}]),
    ?assertEqual(undefined, e2_opt:value(name, Opts)).

int_type_test() ->
    Schema = [{age, [{type, int}]}],
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate([{age, 99}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate([{age, not_an_int}], Schema)).

float_type_test() ->
    Schema = [{age, [{type, float}]}],
    ?assertEqual(9.9, e2_opt:value(age, e2_opt:validate(
                                          [{age, 9.9}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate(
                                  [{age, 99}], Schema)),
    ?assertError({badarg, age}, e2_opt:validate(
                                  [{age, "not a float"}], Schema)).

number_type_test() ->
    Schema = [{age, [{type, number}]}],
    ?assertEqual(9.9, e2_opt:value(age, e2_opt:validate(
                                          [{age, 9.9}], Schema))),
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                          [{age, 99}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate(
                                  [{age, <<"not a number">>}], Schema)).

string_type_test() ->
    Schema = [{name, [{type, string}]}],
    ?assertEqual("Stan", e2_opt:value(
                           name, e2_opt:validate(
                                   [{name, "Stan"}], Schema))),
    ?assertEqual(<<"Stan">>, e2_opt:value(
                               name, e2_opt:validate(
                                       [{name, <<"Stan">>}], Schema))),
    ?assertError({badarg, name}, e2_opt:validate([{name, stan}], Schema)).

atom_type_test() ->
    Schema = [{color, [{type, atom}]}],
    ?assertEqual(blue, e2_opt:value(
                           color, e2_opt:validate(
                                    [{color, blue}], Schema))),
    ?assertError({badarg, color}, e2_opt:validate([{color, "blue"}], Schema)).

list_type_test() ->
    Schema = [{colors, [{type, list}]}],
    ?assertEqual([blue, red], e2_opt:value(
                                colors, e2_opt:validate(
                                         [{colors, [blue, red]}], Schema))),
    ?assertError({badarg, colors}, e2_opt:validate([{colors, blue}], Schema)).

boolean_type_test() ->
    Schema = [{good, [{type, boolean}]}],
    ?assertEqual(true, e2_opt:value(
                         good, e2_opt:validate(
                                 [{good, true}], Schema))),
    ?assertEqual(false, e2_opt:value(
                          good, e2_opt:validate(
                                  [{good, false}], Schema))),
    ?assertEqual(true, e2_opt:value(good, e2_opt:validate([good], Schema))),
    ?assertError({badarg, good}, e2_opt:validate([{good, yes}], Schema)).

function_type_test() ->
    Schema = [{afun, [{type, function}]}],
    Fun = fun() -> ok end,
    ?assertEqual(Fun, e2_opt:value(
                        afun, e2_opt:validate(
                                [{afun, Fun}], Schema))),
    ?assertError({badarg, afun}, e2_opt:validate([{afun, notafun}], Schema)).

binary_type_test() ->
    Schema = [{bin, [{type, binary}]}],
    ?assertEqual(<<13,10>>, e2_opt:value(
                              bin, e2_opt:validate(
                                     [{bin, <<13,10>>}], Schema))),
    ?assertError({badarg, bin}, e2_opt:validate([{bin, "\r\n"}], Schema)).

bad_type_test() ->
    ?assertError({badarg, type},
                 e2_opt:validate([], [{foo, [{type, widget}]}])).

values_test() ->
    Schema = [{gender, [{values, [male, female]}]}],
    ?assertEqual(male, e2_opt:value(gender, e2_opt:validate(
                                              [{gender, male}], Schema))),
    ?assertEqual(female, e2_opt:value(gender, e2_opt:validate(
                                              [{gender, female}], Schema))),
    ?assertError({badarg, gender}, e2_opt:validate([{gender, other}], Schema)).

bad_values_test() ->
    ?assertError({badarg, values},
                 e2_opt:validate([], [{foo, [{values, bad_list}]}])).

min_test() ->
    Schema = [{age, [{min, 0}]}],
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                         [{age, 99}], Schema))),
    ?assertEqual(0, e2_opt:value(age, e2_opt:validate(
                                         [{age, 0}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate([{age, -1}], Schema)).

max_test() ->
    Schema = [{age, [{max, 99}]}],
    ?assertEqual(50, e2_opt:value(age, e2_opt:validate(
                                         [{age, 50}], Schema))),
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                         [{age, 99}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate([{age, 100}], Schema)).

min_max_test() ->
    Schema = [{age, [{min, 0}, {max, 99}]}],
    ?assertEqual(0, e2_opt:value(age, e2_opt:validate(
                                         [{age, 0}], Schema))),
    ?assertEqual(50, e2_opt:value(age, e2_opt:validate(
                                         [{age, 50}], Schema))),
    ?assertEqual(99, e2_opt:value(age, e2_opt:validate(
                                         [{age, 99}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate([{age, -1}], Schema)),
    ?assertError({badarg, age}, e2_opt:validate([{age, 100}], Schema)).

pattern_test() ->
    Schema = [{email, [{pattern, "[a-z]+@[a-z]+\\.[a-z]+"}]}],
    ?assertEqual("dude@car.com", e2_opt:value(
                                   email, e2_opt:validate(
                                            [{email, "dude@car.com"}],
                                            Schema))),
    ?assertError({badarg, email}, e2_opt:validate([{email, "dude"}], Schema)).

bad_pattern_test() ->
    ?assertError({badarg, pattern}, e2_opt:validate(
                                      [], [{email, [{pattern, "[a-z"}]}])).

validate_test() ->
    AgeCheck = fun(Ok) when Ok < 10 -> ok; (_) -> error end,
    Schema = [{age, [{validate, AgeCheck}]}],
    ?assertEqual(0, e2_opt:value(age, e2_opt:validate([{age, 0}], Schema))),
    ?assertError({badarg, age}, e2_opt:validate([{age, 11}], Schema)).

missing_val_test() ->
    Opts = e2_opt:validate([], []),
    ?assertError(badarg, e2_opt:value(name, Opts)).

duplicate_test() ->
    Schema = [foo],
    ?assertError({duplicate, foo}, e2_opt:validate(
                                     [{foo, 1}, {foo, 2}], Schema)).

optional_test() ->
    Schema = [{id, [optional]}],
    Opts = e2_opt:validate([], Schema),
    ?assertEqual("123", e2_opt:value(id, Opts, "123")),
    ?assertError(badarg, e2_opt:value(id, Opts)).

implicit_test() ->
    Schema = [{type, [{values, [dog, cat, lion]}, implicit]}],
    ?assertEqual(dog, e2_opt:value(type, e2_opt:validate([dog], Schema))),
    ?assertEqual(cat, e2_opt:value(type, e2_opt:validate([cat], Schema))),
    ?assertError({required, type}, e2_opt:validate([], Schema)),
    ?assertError({duplicate, type}, e2_opt:validate([dog, cat], Schema)).

multiple_implicit_test() ->
    Schema =
        [{color, [{values, [red, blue]}, implicit]},
         {size, [{values, [large, small]}, implicit]}],
    ?assertEqual(small, e2_opt:value(size, e2_opt:validate(
                                             [small, blue], Schema))),
    ?assertEqual(blue, e2_opt:value(color, e2_opt:validate(
                                             [small, blue], Schema))),
    ?assertError({duplicate_implicit_value,red},
                 e2_opt:validate(
                   [], [{color1, [{values, [blue, red]}, implicit]},
                        {color2, [{values, [red, green]}, implicit]}])).

bad_option_test() ->
    ?assertError({badarg, a_bad_option},
                 e2_opt:validate([], [{foo, [a_bad_option]}])),
    ?assertError({badarg, a_bad_option},
                 e2_opt:validate([], [{foo, [{a_bad_option, "123"}]}])).

usage_test() ->
    Schema = [{name, [{type, string}]},
              {email, [{pattern, "[a-z]+@[a-z]+\\.[a-z]+"},
                       {default, undefined}]},
              {gender, [{values, [male, female]}, {default, unknown}]},
              {age, [{type, int}, {min, 21}, {max, 100}]}],

    Jim = e2_opt:validate([{name, "Jim"},
                           {email, "jim@car.com"},
                           {gender, male},
                           {age, 35}], Schema),
    ?assertEqual("Jim", e2_opt:value(name, Jim)),
    ?assertEqual("jim@car.com", e2_opt:value(email, Jim)),
    ?assertEqual(male, e2_opt:value(gender, Jim)),
    ?assertEqual(35, e2_opt:value(age, Jim)),
    ?assertError(badarg, e2_opt:value(ssn, Jim)),

    Katherin = e2_opt:validate([{name, "Katherin"}, {age, 29}], Schema),
    ?assertEqual("Katherin", e2_opt:value(name, Katherin)),
    ?assertEqual(undefined, e2_opt:value(email, Katherin)),
    ?assertEqual(unknown, e2_opt:value(gender, Katherin)),
    ?assertEqual(29, e2_opt:value(age, Katherin)),

    ok.
