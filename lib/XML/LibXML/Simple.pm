package XML::LibXML::Simple;
use base 'Exporter';
use strict;
use warnings;

our @EXPORT    = qw(XMLin);
our @EXPORT_OK = qw(xml_in);

use XML::LibXML    ();
use File::Slurp    qw/read_file/;
use File::Basename qw/fileparse/;
use File::Spec     ();
use Carp;

use Data::Dumper;  #to be removed

=chapter NAME

XML::LibXML::Simple - XML::LibXML clone of XML::Simple::XMLin()

=chapter SYNOPSIS

  use XML::LibXML::Simple   qw(XMLin);
  my $xml = XMLin <xml file or string>, OPTIONS;

Or the Object Oriented way:

  use XML::LibXML::Simple   ();
  my $xs = XML::Simple->new(OPTIONS);
  my $ref = $xs->XMLin(<xml file or string>, OPTIONS);

=chapter DESCRIPTION

This module is a blunt rewrite of M<XML::Simple> (by Grant McLean) to
use the M<XML::LibXML> parser for XML structures, where the original
uses plain Perl or SAX parsers.

=chapter METHODS

=cut

my %known_opts = map { ($_ => 1) }
  qw(keyattr keeproot forcecontent contentkey noattr searchpath
     forcearray grouptags nsexpand normalisespace normalizespace
     valueattr);

my @DefKeyAttr     = qw(name key id);
my $DefContentKey  = qq(content);

=section Constructors

=c_method new OPTIONS
Instantiate an object, which can be used to call M<XMLin()> on.  You can
provide OPTIONS to this constructor (to be reused for each call to XMLin)
and with each call of XMLin (to be used once)

For XML-DATA and descriptions of the OPTIONS see the L</DETAILS>
section of this manual page.

=cut

sub new(@)
{   my $class = shift;
    my $self  = bless {}, $class;
    $self->{opts} = $self->_take_opts(@_);
    $self;
}

=section Translators

=method XMLin XML-DATA, OPTIONS
For XML-DATA and descriptions of the OPTIONS see the L</DETAILS>
section of this manual page.

=cut

sub XMLin
{   my $self = @_ > 1 && UNIVERSAL::isa($_[0], __PACKAGE__) ? shift
      : __PACKAGE__->new;
    my $target = shift;

    my $this   = $self->_take_opts(@_);
    my $opts   = $self->_init($self->{opts}, $this);

    my $xml    = $self->_get_xml($target, $opts)
        or return;

    my $top  = $self->collapse($xml, $opts);
    if($opts->{keeproot})
    {    my $subtop
           = $opts->{forcearray_always} && ref $top ne 'ARRAY' ? [$top] : $top;
        $top = { $xml->localName => $subtop };
    }

    $top;
}

my $parser;
sub _get_xml($$)
{   my ($self, $source, $opts) = @_;

    $source    = $self->default_data_source($opts) unless defined $source;
    $source    = \*STDIN if $source eq '-';

    $parser  ||= XML::LibXML->new;

    my $xml
      = UNIVERSAL::isa($source,'XML::LibXML::Document') ? $source
      : UNIVERSAL::isa($source,'XML::LibXML::Element') ? $source
      : ref $source eq 'SCALAR'      ? $parser->parse_string($$source)
      : ref $source                  ? $parser->parse_fh($source)
      : $source =~ m{^\s*<.*?>\s*$}s ? $parser->parse_string($source)
      :    $parser->parse_file
              ($self->find_xml_file($source, @{$opts->{searchpath}}));

    $xml = $xml->documentElement
         if $xml->isa('XML::LibXML::Document');

    $xml;
}

sub _take_opts(@)
{   my $self = shift;
   
    my %opts;
    @_ % 2==0
        or die "ERROR: odd number of options.\n";

    while(@_)
    {   my ($key, $val) = (shift, shift);
        my $lkey = lc $key;
        $lkey =~ s/_//g;
        $known_opts{$lkey} or croak "Unrecognised option: $key";
        $opts{$lkey} = $val;
    }

    \%opts;
}

# Returns the name of the XML file to parse if no filename or XML string 
# was provided explictly.

sub default_data_source($)
{   my ($self, $opts) = @_;

    my ($basename, $script_dir, $ext) = fileparse $0, qr[\.[^\.]+];

    # Add script directory to searchpath
    unshift @{$opts->{searchpath}}, $script_dir
        if $script_dir;

    "$basename.xml";
}

sub _init($$)
{   my ($self, $global, $this) = @_;
    my %opt = (%$global, %$this);

    if(defined $opt{contentkey})
         { $opt{collapseagain} = $opt{contentkey} =~ s/^\-// }
    else { $opt{contentkey} = $DefContentKey }

    $opt{normalisespace} ||= $opt{normalizespace} || 0;

    $opt{searchpath} ||= [];
    ref $opt{searchpath} eq 'ARRAY'
        or $opt{searchpath}   = [ $opt{searchpath} ];

    my $fa = delete $opt{forcearray};
    my @fa = ref $fa eq 'ARRAY' ? @$fa : defined $fa ? $fa : ();
    $opt{forcearray_always}   = (@fa==1 && !ref $fa[0] && $fa[0]);
    $opt{forcearray_regex}    = [ grep {ref $_ eq 'Regexp'} @fa ];

    $opt{forcearray_elem}     = {};
    $opt{forcearray_elem}{$_} = 1 for grep !ref $_, @fa;

    # Special cleanup for {keyattr} which could be arrayref or hashref,
    # which behave differently.

    my $ka = $opt{keyattr} || \@DefKeyAttr;
    $ka    = [ $ka ] unless ref $ka;
 
    if(ref $ka eq 'ARRAY')
    {   if(@$ka) { $opt{keyattr} = $ka }
        else { delete $opt{keyattr} }
    }
    elsif(ref $ka eq 'HASH')
    {   # Convert keyattr => { elem => '+attr' }
        # to keyattr => { elem => [ 'attr', '+' ] } 
        my %at;
        while(my($k,$v) = each %$ka)
        {   $v =~ /^(\+|-)?(.*)$/;
            $at{$k} = [ $2, $1 || '' ];
        }
        $opt{keyattr} = \%at;
    }

    # Special cleanup for {valueattr} which could be arrayref or hashref

    my $va = delete $opt{valueattr} || {};
    $va = { map { ($_ => 1) } @$va } if ref $va eq 'ARRAY';
    $opt{valueattrlist} = $va;

    # make sure there's nothing weird in {grouptags}

    !$opt{grouptags} || ref $opt{grouptags} eq 'HASH'
        or croak "Illegal value for 'GroupTags' option -expected a hashref";

    \%opt;
}

sub find_xml_file($@)
{   my ($self, $file) = (shift, shift);
    my @search_path = @_ ? @_ : '.';

    my ($filename, $filedir) = fileparse $file;

    if($filename eq $file)
    {   foreach my $path (@search_path)
        {   my $fullpath = File::Spec->catfile($path, $file);
            return $fullpath if -e $fullpath;
        }
    }
    elsif(-e $file)        # Ignore searchpath if dir component
    {   return $file;
    }

    local $" = ':';
    die "data source $file not found in @search_path\n";
}

sub _add_kv($$$$)
{   my ($d, $k, $v, $opts) = @_;

    if(defined $d->{$k})
    {   # Combine duplicate attributes into arrayref if required
        if(ref $d->{$k} eq 'ARRAY')
             { push @{$d->{$k}}, $v }
        else { $d->{$k} = [ $d->{$k}, $v ] }
    }
    elsif(ref $v eq 'ARRAY')
    {   push @{$d->{$k}}, $v }
    elsif(ref $v eq 'HASH'
       && $k ne $opts->{contentkey} 
       && $opts->{forcearray_always})
    {   push @{$d->{$k}}, $v }
    elsif($opts->{forcearray_elem}{$k}
        || grep {$k =~ $_} @{$opts->{forcearray_regex}}
         )
    {   push @{$d->{$k}}, $v }
    else
    {   $d->{$k} = $v;
    }
    $d->{$k};
}

# Takes the parse tree that XML::Parser produced from the supplied XML and
# recurses through it 'collapsing' unnecessary levels of indirection (nested
# arrays etc) to produce a data structure that is easier to work with.

sub _expand_name($)
{   my $node = shift;
    my $uri  = $node->namespaceURI || '';
    (length $uri ? "{$uri}" : '') . $node->localName;
}

sub collapse($$)
{   my ($self, $xml, $opts) = @_;
    $xml->isa('XML::LibXML::Element') or return;

    my (%data, $text);

    unless($opts->{noattr})
    {   foreach my $attr ($xml->attributes)
        {   my $value = $attr->value;
            $value = $self->normalise_space($value)
                if $opts->{normalisespace}==2;
            my $n = $opts->{nsexpand} ? _expand_name($attr) : $attr->nodeName;
            _add_kv \%data, $n, $value, $opts;
        }
    }
    my $nr_attrs = keys %data;
    my $nr_elems = 0;

  CHILD:
    foreach my $child ($xml->childNodes)
    {
        if($child->isa('XML::LibXML::Element'))
        {   $nr_elems++;
            my $v = $self->collapse($child, $opts);
            my $n = $opts->{nsexpand} ? _expand_name($child) : $child->nodeName;
            _add_kv \%data, $n, $v, $opts if defined $v;
        }
        elsif($child->isa('XML::LibXML::Text'))
        {   $text .= $child->data;
        }
    }

    $text = $self->normalise_space($text)
        if defined $text && $opts->{normalisespace}==2;

    return $opts->{forcecontent} ? { $opts->{contentkey} => $text } : $text
        if $nr_attrs+$nr_elems==0 && defined $text;

    $data{$opts->{contentkey}} = $text
        if defined $text && $nr_elems==0;

    # Roll up 'value' attributes (but only if no nested elements)

    if(keys %data==1)
    {    my $k = (keys %data)[0];
         return $data{$k} if $opts->{valueattrlist}{$k};
    }

    # Turn arrayrefs into hashrefs if key fields present

    if($opts->{keyattr})
    {   while(my ($key,$val) = each %data)
        {   $data{$key} = $self->array_to_hash($key, $val, $opts)
                if ref $val eq 'ARRAY';
        }
    }

    # disintermediate grouped tags

    if(my $gr = $opts->{grouptags})
    {
      ELEMENT:
        while(my ($key, $val) = each %data)
        {   my $sub = $gr->{$key} or next;
            if(ref $val eq 'ARRAY')
            {   next ELEMENT
                    if grep { keys %$_!=1 || !exists $_->{$sub} } @$val;
                $data{$key} = { map { %{$_->{$sub}} } @$val };
            }
            else
            {   ref $val eq 'HASH' && keys %$val==1 or next;
                my ($child_key, $child_val) = %$val;
                $data{$key} = $child_val
                   if $gr->{$key} eq $child_key;
            }
        }
    }

    # Fold hashes containing a single anonymous array up into just the array
    return $data{anon}
        if keys %data == 1
        && exists $data{anon}
        && ref $data{anon} eq 'ARRAY';

    # Roll up named elements with named nested 'value' attributes
    if(my $va = $opts->{valueattrlist})
    {   while(my($key, $val) = each %data)
        {   $va->{$key} && ref $val eq 'HASH' && keys %$val==1 or next;
            $data{$key} = $val->{$va->{$key}};
        }
    }

      $nr_elems+$nr_attrs    ? \%data
    : !defined $text         ? {}
    : $opts->{forcecontent}  ? { $opts->{contentkey} => $text }
    :                          $text;
}

sub normalise_space($)
{   my $self = shift;
    local $_ = shift;
    s/^\s+//s;
    s/\s+$//s;
    s/\s\s+/ /sg;
    $_;
}

# Attempts to 'fold' an array of hashes into an hash of hashes.  Returns a
# reference to the hash on success or the original array if folding is
# not possible.  Behaviour is controlled by 'keyattr' option.
#

sub array_to_hash($$$$)
{   my ($self, $name, $in, $opts) = @_;
    my %out;

    my $ka = $opts->{keyattr} or return $in;

    if(ref $ka eq 'HASH')
    {   my $newkey = $ka->{$name}  or return $in;
        my ($key, $flag) = @$newkey;

        foreach my $h (@$in)
        {   unless(ref $h eq 'HASH' && defined $h->{$key})
            {   warn "<$name> element has no '$key' key attribute\n" if $^W;
                return $in;
            }

            my $val = $h->{$key};
            if(ref $val)
            {   warn "<$name> element has non-scalar '$key' key attribute\n" if $^W;
                return $in;
            }

            $val = $self->normalise_space($val)
                if $opts->{normalisespace}==1;

            warn "<$name> element has non-unique value in '$key' "
               . "key attribute: $val\n" if $^W && defined $out{$val};

            $out{$val} = { %$h };
            $out{$val}{"-$key"} = $out{$val}{$key} if $flag eq '-';
            delete $out{$val}{$key} if $flag ne '+';
        }
    }

    else  # Arrayref
    {   my $default_keys = "@DefKeyAttr" eq "@$ka";

      ELEMENT:
        foreach my $h (@$in)
        {   ref $h eq 'HASH' or return $in;

            foreach my $key (@$ka)
            {   my $val = $h->{$key};
                defined $val or next;

                if(ref $val)
                {   warn "<$name> element has non-scalar '$key' key attribute"
                        if $^W && ! $default_keys;
                    return $in;
                }

                $val = $self->normalise_space($val)
                    if $opts->{normalisespace} == 1;

                warn "<$name> element has non-unique value in '$key' "
                   . "key attribute: $val" if $^W && $out{$val};

                $out{$val} = { %$h };
                delete $out{$val}{$key};
                next ELEMENT;
            }
            return $in;    # No keyfield matched
        }
    }

    $opts->{collapseagain}
        or return \%out;

    # avoid over-complicated structures like
    # dir => { libexecdir    => { content => '$exec_prefix/libexec' },
    #          localstatedir => { content => '$prefix' },
    #        }
    # into
    # dir => { libexecdir    => '$exec_prefix/libexec',
    #          localstatedir => '$prefix',
    #        }

    my $contentkey = $opts->{contentkey};

    # first go through the values, checking that they are fit to collapse
    foreach my $v (values %out)
    {   next if ref $v eq 'HASH' && keys %$v == 1 && $v->{$contentkey};
        return \%out;
    }

    $out{$_} =  $out{$_}{$contentkey} for keys %out;
    \%out;
}
  
1;

__END__

=chapter FUNCTIONS

The functions C<XMLin> (exported implictly) and C<xml_in>
(exported on request) simply call C<< XML::Simple->new->XMLin() >>
with the provided parameters.

=chapter DETAILS

=section Differences with XML::Simple

In general, the output and the options are equivalent, although this
module has some differences with M<XML::Simple> to be aware of.

=over 4

=item .

Only M<XMLin()> is supported: if you want to write XML the use a schema
(for instance with M<XML::Compile>).  Do not attempt to create XML by
hand!  If you still think you need it, then have a look at XMLout() as
implemented by M<XML::Simple> or any of a zillion template systems.

=item .

IMO, you should use a templating system if you want variables filled-in
in the input: it is not a task for this module.

=item .

Also, empty elements are not removed: being empty has a meaning which
should not be ignored.

=item .

There are a few small differences in the result of the C<forcearray> option,
because XML::Simple seems to behave inconsequently.

=back

=section Parameter XML-DATA

As first parameter to M<XMLin()> must provide the XML message to be
translated into a Perl structure.  Choose one of the following:

=over 4

=item A filename

If the filename contains no directory components, C<XMLin()> will look for the
file in each directory in the SearchPath (see OPTIONS below) and in the
current directory.  eg:

  $ref = XMLin('/etc/params.xml');

Note, the filename C<< - >> (dash) can be used to parse from STDIN.

=item undef

If there is no XML specifier, C<XMLin()> will check the script directory and
each of the SearchPath directories for a file with the same name as the script
but with the extension '.xml'.  Note: if you wish to specify options, you
must specify the value 'undef'.  eg:

  $ref = XMLin(undef, ForceArray => 1);

=item A string of XML

A string containing XML (recognised by the presence of '<' and '>' characters)
will be parsed directly.  eg:

  $ref = XMLin('<opt username="bob" password="flurp" />');

=item An IO::Handle object

An IO::Handle object will be read to EOF and its contents parsed. eg:

  $fh = IO::File->new('/etc/params.xml');
  $ref = XMLin($fh);

=back

=section OPTIONS

M<XML::LibXML::Simple> supports most options defined by M<XML::Simple>, so
the interface is quite compatible.  Minor changes apply.  This explanation
is extracted from the XML::Simple manual-page.

=over 4

=item *

check out C<ForceArray> because you'll almost certainly want to turn it on

=item *

make sure you know what the C<KeyAttr> option does and what its default
value is because it may surprise you otherwise.

=item *

Option names are case in-sensitive so you can use the mixed case versions
shown here; you can add underscores between the words (eg: key_attr)
if you like.

=back

In alphabetic order:

=over 4

=item ContentKey => 'keyname' I<# seldom used>

When text content is parsed to a hash value, this option let's you specify a
name for the hash key to override the default 'content'.  So for example:

  XMLin('<opt one="1">Text</opt>', ContentKey => 'text')

will parse to:

  { 'one' => 1, 'text' => 'Text' }

instead of:

  { 'one' => 1, 'content' => 'Text' }

You can also prefix your selected key name with a '-' character to have 
C<XMLin()> try a little harder to eliminate unnecessary 'content' keys after
array folding.  For example:

  XMLin(
    '<opt><item name="one">First</item><item name="two">Second</item></opt>', 
    KeyAttr => {item => 'name'}, 
    ForceArray => [ 'item' ],
    ContentKey => '-content'
  )

will parse to:

  {
    'item' => {
      'one' =>  'First'
      'two' =>  'Second'
    }
  }

rather than this (without the '-'):

  {
    'item' => {
      'one' => { 'content' => 'First' }
      'two' => { 'content' => 'Second' }
    }
  }

=item ForceArray => 1 I<# important>

This option should be set to '1' to force nested elements to be represented
as arrays even when there is only one.  Eg, with ForceArray enabled, this
XML:

    <opt>
      <name>value</name>
    </opt>

would parse to this:

    {
      'name' => [
                  'value'
                ]
    }

instead of this (the default):

    {
      'name' => 'value'
    }

This option is especially useful if the data structure is likely to be written
back out as XML and the default behaviour of rolling single nested elements up
into attributes is not desirable. 

If you are using the array folding feature, you should almost certainly
enable this option.  If you do not, single nested elements will not be
parsed to arrays and therefore will not be candidates for folding to a
hash.  (Given that the default value of 'KeyAttr' enables array folding,
the default value of this option should probably also have been enabled
as well).

=item ForceArray => [ names ] I<# important>

This alternative (and preferred) form of the 'ForceArray' option allows you to
specify a list of element names which should always be forced into an array
representation, rather than the 'all or nothing' approach above.

It is also possible to include compiled regular
expressions in the list --any element names which match the pattern
will be forced to arrays.  If the list contains only a single regex,
then it is not necessary to enclose it in an arrayref.  Eg:

  ForceArray => qr/_list$/

=item ForceContent => 1 I<# seldom used>

When C<XMLin()> parses elements which have text content as well as attributes,
the text content must be represented as a hash value rather than a simple
scalar.  This option allows you to force text content to always parse to
a hash value even when there are no attributes.  So for example:

  XMLin('<opt><x>text1</x><y a="2">text2</y></opt>', ForceContent => 1)

will parse to:

  {
    'x' => {           'content' => 'text1' },
    'y' => { 'a' => 2, 'content' => 'text2' }
  }

instead of:

  {
    'x' => 'text1',
    'y' => { 'a' => 2, 'content' => 'text2' }
  }

=item GroupTags => { grouping tag => grouped tag } I<# handy>

You can use this option to eliminate extra levels of indirection in your Perl
data structure.  For example this XML:

  <opt>
   <searchpath>
     <dir>/usr/bin</dir>
     <dir>/usr/local/bin</dir>
     <dir>/usr/X11/bin</dir>
   </searchpath>
 </opt>

Would normally be read into a structure like this:

  {
    searchpath => {
                    dir => [ '/usr/bin', '/usr/local/bin', '/usr/X11/bin' ]
                  }
  }

But when read in with the appropriate value for 'GroupTags':

  my $opt = XMLin($xml, GroupTags => { searchpath => 'dir' });

It will return this simpler structure:

  {
    searchpath => [ '/usr/bin', '/usr/local/bin', '/usr/X11/bin' ]
  }

The grouping element (C<< <searchpath> >> in the example) must not contain any
attributes or elements other than the grouped element.

You can specify multiple 'grouping element' to 'grouped element' mappings in
the same hashref.  If this option is combined with C<KeyAttr>, the array
folding will occur first and then the grouped element names will be eliminated.

=item KeepRoot => 1 I<# handy>

In its attempt to return a data structure free of superfluous detail and
unnecessary levels of indirection, C<XMLin()> normally discards the root
element name.  Setting the 'KeepRoot' option to '1' will cause the root element
name to be retained.  So after executing this code:

  $config = XMLin('<config tempdir="/tmp" />', KeepRoot => 1)

You'll be able to reference the tempdir as
C<$config-E<gt>{config}-E<gt>{tempdir}> instead of the default
C<$config-E<gt>{tempdir}>.

=item KeyAttr => [ list ] I<# important>

This option controls the 'array folding' feature which translates nested
elements from an array to a hash.  It also controls the 'unfolding' of hashes
to arrays.

For example, this XML:

    <opt>
      <user login="grep" fullname="Gary R Epstein" />
      <user login="stty" fullname="Simon T Tyson" />
    </opt>

would, by default, parse to this:

    {
      'user' => [
                  {
                    'login' => 'grep',
                    'fullname' => 'Gary R Epstein'
                  },
                  {
                    'login' => 'stty',
                    'fullname' => 'Simon T Tyson'
                  }
                ]
    }

If the option 'KeyAttr => "login"' were used to specify that the 'login'
attribute is a key, the same XML would parse to:

    {
      'user' => {
                  'stty' => {
                              'fullname' => 'Simon T Tyson'
                            },
                  'grep' => {
                              'fullname' => 'Gary R Epstein'
                            }
                }
    }

The key attribute names should be supplied in an arrayref if there is more
than one.  C<XMLin()> will attempt to match attribute names in the order
supplied.

Note 1: The default value for 'KeyAttr' is C<< ['name', 'key', 'id'] >>.
If you do not want folding on input or unfolding on output you must
setting this option to an empty list to disable the feature.

Note 2: If you wish to use this option, you should also enable the
C<ForceArray> option.  Without 'ForceArray', a single nested element will be
rolled up into a scalar rather than an array and therefore will not be folded
(since only arrays get folded).

=item KeyAttr => { list } I<# important>

This alternative (and preferred) method of specifiying the key attributes
allows more fine grained control over which elements are folded and on which
attributes.  For example the option 'KeyAttr => { package => 'id' } will cause
any package elements to be folded on the 'id' attribute.  No other elements
which have an 'id' attribute will be folded at all. 

Two further variations are made possible by prefixing a '+' or a '-' character
to the attribute name:

The option 'KeyAttr => { user => "+login" }' will cause this XML:

    <opt>
      <user login="grep" fullname="Gary R Epstein" />
      <user login="stty" fullname="Simon T Tyson" />
    </opt>

to parse to this data structure:

    {
      'user' => {
                  'stty' => {
                              'fullname' => 'Simon T Tyson',
                              'login'    => 'stty'
                            },
                  'grep' => {
                              'fullname' => 'Gary R Epstein',
                              'login'    => 'grep'
                            }
                }
    }

The '+' indicates that the value of the key attribute should be copied
rather than moved to the folded hash key.

A '-' prefix would produce this result:

    {
      'user' => {
                  'stty' => {
                              'fullname' => 'Simon T Tyson',
                              '-login'    => 'stty'
                            },
                  'grep' => {
                              'fullname' => 'Gary R Epstein',
                              '-login'    => 'grep'
                            }
                }
    }

=item NoAttr => 1 I<# handy>

When used with C<XMLin()>, any attributes in the XML will be ignored.

=item NormaliseSpace => 0 | 1 | 2 I<# handy>

This option controls how whitespace in text content is handled.  Recognised
values for the option are:

=over 4

=item 0

(default) whitespace is passed through unaltered (except of course for the
normalisation of whitespace in attribute values which is mandated by the XML
recommendation)

=item 1

whitespace is normalised in any value used as a hash key (normalising means
removing leading and trailing whitespace and collapsing sequences of whitespace
characters to a single space)

=item 2

whitespace is normalised in all text content

=back

Note: you can spell this option with a 'z' if that is more natural for you.

=item SearchPath => [ list ] I<# handy>

If you pass C<XMLin()> a filename, but the filename include no directory
component, you can use this option to specify which directories should be
searched to locate the file.  You might use this option to search first in the
user's home directory, then in a global directory such as /etc.

If a filename is provided to C<XMLin()> but SearchPath is not defined, the
file is assumed to be in the current directory.

If the first parameter to C<XMLin()> is undefined, the default SearchPath
will contain only the directory in which the script itself is located.
Otherwise the default SearchPath will be empty.  

=item ValueAttr => [ names ] I<# in - handy>

Use this option to deal elements which always have a single attribute and no
content.  Eg:

  <opt>
    <colour value="red" />
    <size   value="XXL" />
  </opt>

Setting C<< ValueAttr => [ 'value' ] >> will cause the above XML to parse to:

  {
    colour => 'red',
    size   => 'XXL'
  }

instead of this (the default):

  {
    colour => { value => 'red' },
    size   => { value => 'XXL' }
  }

=back

=chapter EXAMPLES

When C<XMLin()> reads the following very simple piece of XML:

    <opt username="testuser" password="frodo"></opt>

it returns the following data structure:

    {
      'username' => 'testuser',
      'password' => 'frodo'
    }

The identical result could have been produced with this alternative XML:

    <opt username="testuser" password="frodo" />

Or this (although see 'ForceArray' option for variations):

    <opt>
      <username>testuser</username>
      <password>frodo</password>
    </opt>

Repeated nested elements are represented as anonymous arrays:

    <opt>
      <person firstname="Joe" lastname="Smith">
        <email>joe@smith.com</email>
        <email>jsmith@yahoo.com</email>
      </person>
      <person firstname="Bob" lastname="Smith">
        <email>bob@smith.com</email>
      </person>
    </opt>

    {
      'person' => [
                    {
                      'email' => [
                                   'joe@smith.com',
                                   'jsmith@yahoo.com'
                                 ],
                      'firstname' => 'Joe',
                      'lastname' => 'Smith'
                    },
                    {
                      'email' => 'bob@smith.com',
                      'firstname' => 'Bob',
                      'lastname' => 'Smith'
                    }
                  ]
    }

Nested elements with a recognised key attribute are transformed (folded) from
an array into a hash keyed on the value of that attribute (see the C<KeyAttr>
option):

    <opt>
      <person key="jsmith" firstname="Joe" lastname="Smith" />
      <person key="tsmith" firstname="Tom" lastname="Smith" />
      <person key="jbloggs" firstname="Joe" lastname="Bloggs" />
    </opt>

    {
      'person' => {
                    'jbloggs' => {
                                   'firstname' => 'Joe',
                                   'lastname' => 'Bloggs'
                                 },
                    'tsmith' => {
                                  'firstname' => 'Tom',
                                  'lastname' => 'Smith'
                                },
                    'jsmith' => {
                                  'firstname' => 'Joe',
                                  'lastname' => 'Smith'
                                }
                  }
    }


The <anon> tag can be used to form anonymous arrays:

    <opt>
      <head><anon>Col 1</anon><anon>Col 2</anon><anon>Col 3</anon></head>
      <data><anon>R1C1</anon><anon>R1C2</anon><anon>R1C3</anon></data>
      <data><anon>R2C1</anon><anon>R2C2</anon><anon>R2C3</anon></data>
      <data><anon>R3C1</anon><anon>R3C2</anon><anon>R3C3</anon></data>
    </opt>

    {
      'head' => [
                  [ 'Col 1', 'Col 2', 'Col 3' ]
                ],
      'data' => [
                  [ 'R1C1', 'R1C2', 'R1C3' ],
                  [ 'R2C1', 'R2C2', 'R2C3' ],
                  [ 'R3C1', 'R3C2', 'R3C3' ]
                ]
    }

Anonymous arrays can be nested to arbirtrary levels and as a special case, if
the surrounding tags for an XML document contain only an anonymous array the
arrayref will be returned directly rather than the usual hashref:

    <opt>
      <anon><anon>Col 1</anon><anon>Col 2</anon></anon>
      <anon><anon>R1C1</anon><anon>R1C2</anon></anon>
      <anon><anon>R2C1</anon><anon>R2C2</anon></anon>
    </opt>

    [
      [ 'Col 1', 'Col 2' ],
      [ 'R1C1', 'R1C2' ],
      [ 'R2C1', 'R2C2' ]
    ]

Elements which only contain text content will simply be represented as a
scalar.  Where an element has both attributes and text content, the element
will be represented as a hashref with the text content in the 'content' key
(see the C<ContentKey> option):

  <opt>
    <one>first</one>
    <two attr="value">second</two>
  </opt>

  {
    'one' => 'first',
    'two' => { 'attr' => 'value', 'content' => 'second' }
  }

Mixed content (elements which contain both text content and nested elements)
will be not be represented in a useful way - element order and significant
whitespace will be lost.  If you need to work with mixed content, then
XML::Simple is not the right tool for your job - check out the next section.

=chapter SEE ALSO

L<XML::Compile> for processing XML when a schema is available

L<XML::Simple>, the SAX and original implementation

=cut
