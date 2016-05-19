#!/usr/bin/perl -w
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#
#    * Neither the name of ZanyBlue nor the names of its contributors may
#      be used to endorse or promote products derived from this software
#      without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# Simple CGI Perl script to execute the Text example applications.

use strict;
use CGI;
use FileHandle;
use File::Basename;

my $tmpldir = dirname ($0);
my $instdir = "$tmpldir/.bin";
my %examples = (
    "curtime"    => 1,
    "encodings"  => 1,
    "formatting" => 1,
    "jenkins"    => 1,
    "tomcat"     => 1,
);

my %locales = (
    "Arabic"                  => "ar",
    "Czech"                   => "cs",
    "Danish"                  => "da",
    "German"                  => "de",
    "Greek"                   => "el",
    "English"                 => "en",
    "English (Australia)"     => "en_AU",
    "English (Canada)"        => "en_CA",
    "English (Ireland)"       => "en_IE",
    "English (Great Britian)" => "en_GB",
    "English (New Zealand)"   => "en_NZ",
    "English (South Africa)"  => "en_ZA",
    "Spanish"                 => "es",
    "Finnish"                 => "fi",
    "French"                  => "fr",
    "Irish"                   => "ga",
    "Hebrew"                  => "he",
    "Hungarian"               => "hu",
    "Italian"                 => "it",
    "Japanese"                => "ja",
    "Korean"                  => "ko",
    "Norwegian"               => "nb",
    "Dutch"                   => "nl",
    "Polish"                  => "pl",
    "Portuguese"              => "pt",
    "Romanian"                => "ro",
    "Russian"                 => "ru",
    "Slovak"                  => "sk",
    "Swedish"                 => "sv",
    "Thai"                    => "th",
    "Turkish"                 => "tr",
    "Chinese (Simplified)"    => "zh",
    "Chinese (Traditional)"   => "zh_Hant",
);

my %pseudo_options = (
    "None"                  => "n",
    "Enclosed_Alphanumeric" => "e",
    "Halfwidth_Forms"       => "h",
    "Lowercase"             => "l",
    "Uppercase"             => "u",
);

my $cgi = new CGI ();
my $params = $cgi->Vars;
if ($cgi->param ('dumpapps')) {
    dumpapps (\%examples);
}
my $template = sprintf ("%s/examples.html", $tmpldir);
my $application = get_param ($params, 'application', "", keys (%examples));
my $locale = get_param ($params, 'locale', "en", values (%locales));
my $pseudo = get_param ($params, 'pseudo', "n", values (%pseudo_options));
my $html = expand_template($application, $template, $locale, $pseudo,
                           \%examples, \%locales, \%pseudo_options);
print $cgi->header(-charset => "UTF-8");
printf ("%s\n", $html);

############
# dumpapps #
############

sub dumpapps {
    my $examples = shift;
    foreach my $application (sort (keys (%{$examples}))) {
        printf ("%s\n", $application);
    }
    exit (0);
}

###################
# execute_command #
###################

sub execute_command {
    my $name = shift;
    my $command = shift;
    my @result = ();
    my $fh = new FileHandle ("$command 2>&1|");
    if (not defined ($fh)) {
        my $errmsg = sprintf ("Failed to execute \"%s\" application: %s\n", $name, "$!");
        push (@result, $errmsg);
    }
    else {
        while (<$fh>) {
            push (@result, $_);
        }
        $fh->close ();
    }
    return \@result;
}

###################
# expand_template #
###################

sub expand_template {
    my $application = shift;
    my $template = shift;
    my $locale = shift;
    my $pseudo = shift;
    my $examples = shift;
    my $locales = shift;
    my $pseudo_options = shift;
    my $title = "";
    if ($application) {
        my $pseudo_name = "";
        foreach my $name (keys (%pseudo_options)) {
            if ($pseudo eq $pseudo_options{$name}) {
                $pseudo_name = $name;
            }
        }
        $title = " &gt; $application $locale $pseudo_name";
    }
    my $example_html = make_example_options ($application, $examples);
    my $language_html = make_language_options ($locale, $locales);
    my $pseudo_html = make_pseudo_options ($pseudo, $pseudo_options);
    my $executable = locate_executable ($application);
    my $command = generate_command ($executable, $locale, $pseudo);
    my $description = generate_description ($application, $executable);
    my $output = generate_output ($application, $command);
    $command =~ s:.*/::;
    my $fh = new FileHandle($template);
    my $result = "";
    my $printing = 1;
    while (<$fh>) {
        $printing = 0 if (/<!-- BEGIN-OUTPUT -->/ and not $output);
        s/<!-- TITLE -->/$title/;
        s/<!-- EXAMPLE_OPTIONS -->/$example_html/;
        s/<!-- LANGUAGE_OPTIONS -->/$language_html/;
        s/<!-- PSEUDO_OPTIONS -->/$pseudo_html/;
        s/<!-- COMMAND -->/    \$ $command/;
        s/<!-- DESCRIPTION -->/$description/;
        s/<!-- OUTPUT -->/$output/;
        s/<!-- LOCALE -->/$locale/;
        $result .= $_ if ($printing);
        $printing = 1 if (/<!-- END-OUTPUT -->/);
    }
    $fh->close();
    return $result;
}

####################
# generate_command #
####################

sub generate_command {
    my $executable = shift;
    my $locale = shift;
    my $pseudo = shift;
    my $result = "";
    if ($executable) {
        $result = "$executable -l$locale -x$pseudo";
    }
    return $result;
}

########################
# generate_description #
########################

sub generate_description {
    my $application = shift;
    my $executable = shift;
    my $result = "";
    if ($executable) {

        my $output = execute_command ($application, "$executable -h");
        my $inlist = 0;
        foreach (@{$output}) {
            if (/^\s*\* /) {
                if (not $inlist) {
                    $inlist = 1;
                    $result .= "<ul>\n";
                }
                else {
                    $result .= "</li>\n";
                }
                $result .= "<li>\n";
                s/^\s*\* //;
            }
            if (/^\s*$/) {
                $result .= "</li>\n</ul>\n" if ($inlist);
                $result .= "<p />\n";
            }
            $result .= $_;
        }
    }
    return $result;
}

###################
# generate_output #
###################

sub generate_output {
    my $application = shift;
    my $command = shift;
    my $result = "";
    if (not $command) {
        return $result;
    }
    my $output = execute_command ($application, $command);
    foreach (@{$output}) {
        $result .= "    $_";
    }
    return $result;
}

#############
# get_param #
#############

sub get_param {
    my $params = shift;
    my $param_name = shift;
    my $defvalue = shift;
    my @valid_set = @_;
    my $result = $params->{$param_name} || "";
    foreach my $valid (@valid_set) {
        return $result if ($result eq $valid);
    }
    return $defvalue;
}

#####################
# locate_executable #
#####################

sub locate_executable {
    my $application = shift;
    my $result = "";
    if (exists $examples{$application}) {
        $result = "$instdir/x_$application";
    }
    return $result;
}

########################
# make_example_options #
########################

sub make_example_options {
    my $selected = shift;
    my $examples = shift;
    my $result = "";
    foreach my $example (sort (keys (%{$examples}))) {
        $result .= "  <option value=\"$example\"";
        $result .= " selected" if ($example eq $selected);
        $result .= ">$example</option>\n";
    }
    return $result;
}

#########################
# make_language_options #
#########################

sub make_language_options {
    my $locale = shift;
    my $locales = shift;
    my $result = "";
    foreach my $language (sort (keys %{$locales})) {
        my $tag = $locales->{$language};
        $result .= "  <option value=\"$tag\"";
        $result .= " selected" if ($tag eq $locale);
        $result .= ">$language</option>\n";
    }
    return $result;
}

#######################
# make_pseudo_options #
#######################

sub make_pseudo_options {
    my $pseudo = shift;
    my $pseudo_type = shift;
    my $result = "";
    foreach my $name (sort (keys (%{$pseudo_type}))) {
        my $tag = $pseudo_type->{$name};
        $result .= "  <option value=\"$tag\"";
        $result .= " selected" if ($tag eq $pseudo);
        $result .= ">$name</option>\n";
    }
    return $result;
}
