#!/usr/bin/perl

use CGI qw/:standard/;

print header,
    start_html(-title=>'A Simple Form', -bgcolor=>"#FFFFCC"),
    h1('This is a simple form to demonstrate CGI'),
    start_form,

    table(
	Tr(td("Given name"), td(
	    textfield(-name=>"given", -size=>30)
	)),

	Tr(td("Family name"), td(
	    textfield(-name=>"family", -size=>50)
	)),

	Tr(td(
	    submit(-name=>"Submit", -value=>"Send",
	    	-style=>"color: red; background: #F5FFFA"),
	    reset(-value=>"Clear", -style=>"color: green; background: #F5FFFA"),
	)),
	),

    end_form,
    hr;

if (param()) {
   print "Your given name is ",em(param('given')),p,
	 "Your family name is ",em(param('family')),
	 hr;
}

print
    end_html;
