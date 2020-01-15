use ExtUtils::MakeMaker;

use 5.010;

WriteMakefile
 ( NAME	 	=> 'XML::LibXML::Simple'
 , VERSION	=> '1.00'
 , PREREQ_PM    =>
   { XML::LibXML       => 1.64
   , Test::More        => 0.54
   , Scalar::Util      => 0
   }
 , AUTHOR       => 'Mark Overmeer'
 , ABSTRACT     => 'XML::LibXML alternative to XML::Simple::XMLin()'
 , LICENSE      => 'perl_5'

 , META_MERGE   =>
      { 'meta-spec' => { version => 2 }
      , resources  =>
          { repository =>
              { type => 'git'
              , url  => 'https://github.com/markov2/perl5-XML-LibXML-Simple.git'
              , web  => 'https://github.com/markov2/perl5-XML-LibXML-Simple'
              }
          , homepage => 'http://perl.overmeer.net/CPAN/'
          , license  => [ 'http://dev.perl.org/licenses/' ]
          }
      }
 );

### used by oodist during production of distribution
sub MY::postamble { <<'__POSTAMBLE' }

RAWDIR          = ../public_html/xml-libxml-simple/raw
DISTDIR         = ../public_html/xml-libxml-simple/source
SKIP_LINKS      = XML::LibXML

# for POD
FIRST_YEAR      = 2008
EMAIL           = markov@cpan.org
WEBSITE         = http://perl.overmeer.net/xml-libxml-simple/
__POSTAMBLE
