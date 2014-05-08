pfile
=====

Yet another config parser

Dependencies
. rebar
. make

Build
To build the pfile library just run make.
$ make


example:

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
 

