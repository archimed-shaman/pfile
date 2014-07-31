pfile
=====

Yet another config parser.

Dependencies
 - rebar
 - make

Build

To build the pfile library just run make.
$ make


Example:

```
#first anonymous section
list = {1;2;3;4;5;6}; # list of values

option_list = 
{
  key1 = 1;
  key2 = "string";
};

complex_option_list = 
{
  {
    key1 = 1;
    key2 = "string";
  };
  {
    key1 = 2;
    key2 = "text";
  };
};

[first_section]
key1 = 1;
key2 = "string";

[optionset]
{
  id = 1;
  name = "the \"first\"";
};
{
  id = 2;
  name = 
"the second
with
wrapped
name
";
};
``` 

Other examples can be founded in priv:

```erlang
{ok, Data} = file:read_file("../priv/config.cfg").
pfile:parse(binary_to_list(Data)).
```

Testing

There is a possible bug in line counting. So, unit test a not valid now. Waiting for pull request approvement: https://github.com/erlang/otp/pull/431
