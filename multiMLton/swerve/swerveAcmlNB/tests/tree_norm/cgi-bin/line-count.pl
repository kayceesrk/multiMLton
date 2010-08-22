#!/usr/bin/perl

use CGI qw/:standard/;

print header,
    start_html(-title=>'Count Lines', -bgcolor=>"#FFFFCC"),
    h1('This counts the lines in a file to demonstrate uploading.'),
    start_multipart_form,

    table(
	Tr(td("File Name"), td(
	   filefield(-name=>'uploaded_file',
                            -size=>50,
                            -maxlength=>80) 
	)),

	Tr(td(
	    submit(-name=>"Submit", -value=>"Send",
	    	-style=>"color: red; background: #F5FFFA"),
	    reset(-value=>"Clear", -style=>"color: green; background: #F5FFFA"),
	)),
	),

    end_form,
    hr;

if (param())
{
    $n = 0;
    $fh = upload('uploaded_file');

    $filename = param('uploaded_file');
    $type = uploadInfo($filename)->{'Content-Type'};

    if ($type ne 'text/plain')
    {
	print "Only plain text files are allowed.";
    }
    else
    {
	while (<$fh>)
	{
	   $n++;
	}

	print "The length of the file is $n lines";
    }

    print hr;
}

print
    end_html;
