package XML::LibXML::Simple;
use base 'Exporter';

use strict;
use warnings;

our @EXPORT    = qw(XMLin);
our @EXPORT_OK = qw(xml_in);

use XML::LibXML       ();
use File::Slurp::Tiny qw/read_file/;
use File::Basename    qw/fileparse/;
use File::Spec        ();
use Carp;
use Scalar::Util      qw/blessed/;

use Data::Dumper;  #to be removed

=chapter NAME

XML::LibXML::Simple - XML::LibXML clone of XML::Simple::XMLin()

=chapter SYNOPSIS

  my $xml  = ...;  # filename, fh, string, or XML::LibXML-node

Imperative:

  use XML::LibXML::Simple   qw(XMLin);
  my $data = XMLin $xml, %options;

Or the Object Oriented way:

  use XML::LibXML::Simple   ();
  my $xs   = XML::LibXML::Simple->new(%options);
  my $data = $xs->XMLin($xml, %options);

=chapter DESCRIPTION

This module is a blunt rewrite of M<XML::Simple> (by Grant McLean) to
use the M<XML::LibXML> parser for XML structures, where the original
uses plain Perl or SAX parsers.

=chapter METHODS

=cut

my %known_opts = map +($_ => 1),
  qw(keyattr keeproot forcecontent contentkey noattr searchpath
     forcearray grouptags nsexpand normalisespace normalizespace
     valueattr nsstrip parser parseropts);

my @default_attributes  = qw(name key id);
my $default_content_key = 'content';

#-------------
=section Constructors

=c_method new %options
Instantiate an object, which can be used to call M<XMLin()> on.  You can
provide %options to this constructor (to be reused for each call to XMLin)
and with each call of XMLin (to be used once)

For descriptions of the %options see the L</DETAILS>
section of this manual page.

=cut

sub new(@)
{   my $class = shift;
    my $self  = bless {}, $class;
    my $opts  = $self->{opts} = $self->_take_opts(@_);

    # parser object cannot be reused
    !defined $opts->{parser}
        or error __x"parser option for XMLin only";

    $self;
}

#-------------
=section Translators

=method XMLin $xmldata, %options
For $xmldata and descriptions of the %options see the L</DETAILS>
section of this manual page.

=cut

sub XMLin
{   my $self = @_ > 1 && blessed $_[0] && $_[0]->isa(__PACKAGE__) ? shift
      : __PACKAGE__->new;
    my $target = shift;

    my $this = $self->_take_opts(@_);
    my $opts = $self->_init($self->{opts}, $this);

    my $xml  = $self->_get_xml($target, $opts)
        or return;

    if(my $cb = $opts->{hooknodes})
    {   $self->{XCS_hooks} = $cb->($self, $xml);
    }

    my $top  = $self->collapse($xml, $opts);
    if($opts->{keeproot})
    {   my $subtop
          = $opts->{forcearray_always} && ref $top ne 'ARRAY' ? [$top] : $top;
        $top = +{ $xml->localName => $subtop };
    }

    $top;
}
*xml_in = \&XMLin;

sub _get_xml($$)
{   my ($self, $source, $opts) = @_;

    $source    = $self->default_data_source($opts)
        unless defined $source;

    $source    = \*STDIN
        if $source eq '-';

    my $parser = $opts->{parser}
              || $self->_create_parser($opts->{parseropts});

    my $xml
      = blessed $source &&
        (  $source->isa('XML::LibXML::Document')
        || $source->isa('XML::LibXML::Element' )) ? $source
      : ref $source eq 'SCALAR' ? $parser->parse_string($$source)
      : ref $source             ? $parser->parse_fh($source)
      : $source =~ m{^\s*\<.*?\>\s*$}s ? $parser->parse_string($source)
      :    $parser->parse_file
              ($self->find_xml_file($source, @{$opts->{searchpath}}));

    $xml = $xml->documentElement
         if $xml->isa('XML::LibXML::Document');

    $xml;
}

sub _create_parser(@)
{   my $self = shift;
    my @popt = @_ != 1 ? @_ : ref $_[0] eq 'HASH' ? %{$_[0]} : @{$_[0]};

    XML::LibXML->new
      ( line_numbers    => 1
      , no_network      => 1
      , expand_xinclude => 0
      , expand_entities => 1
      , load_ext_dtd    => 0
      , ext_ent_handler =>
           sub { alert __x"parsing external entities disabled"; '' }
      , @popt
      );
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
    else { $opt{contentkey} = $default_content_key }

    $opt{normalisespace} ||= $opt{normalizespace} || 0;

    $opt{searchpath} ||= [];
    ref $opt{searchpath} eq 'ARRAY'
        or $opt{searchpath} = [ $opt{searchpath} ];

    my $fa = delete $opt{forcearray} || 0;
    my (@fa_regex, %fa_elem);
    if(ref $fa)
     {   foreach (ref $fa eq 'ARRAY' ? @$fa : $fa)
         {   if(ref $_ eq 'Regexp') { push @fa_regex, $_ }
             else { $fa_elem{$_} = 1 }
         }
    }
    else { $opt{forcearray_always} = $fa }
    $opt{forcearray_regex} = \@fa_regex;
    $opt{forcearray_elem}  = \%fa_elem;

    # Special cleanup for {keyattr} which could be arrayref or hashref,
    # which behave differently.

    my $ka = $opt{keyattr} || \@default_attributes;
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
    $va = +{ map +($_ => 1), @$va } if ref $va eq 'ARRAY';
    $opt{valueattrlist} = $va;

    # make sure there's nothing weird in {grouptags}

    !$opt{grouptags} || ref $opt{grouptags} eq 'HASH'
         or croak "Illegal value for 'GroupTags' option -expected a hashref";

    $opt{parseropts} ||= {};

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
        if(ref $d->{$k} eq 'ARRAY')   { push @{$d->{$k}}, $v }
        else                          { $d->{$k} = [ $d->{$k}, $v ] } }
    elsif(ref $v eq 'ARRAY')          { push @{$d->{$k}}, $v }
    elsif(ref $v eq 'HASH'
       && $k ne $opts->{contentkey} 
       && $opts->{forcearray_always}) { push @{$d->{$k}}, $v }
    elsif($opts->{forcearray_elem}{$k}
        || grep $k =~ $_, @{$opts->{forcearray_regex}}
         )                            { push @{$d->{$k}}, $v }
    else                              { $d->{$k} = $v }
    $d->{$k};
}

# Takes the parse tree that XML::LibXML::Parser produced from the supplied
# XML and recurse through it 'collapsing' unnecessary levels of indirection
# (nested arrays etc) to produce a data structure that is easier to work with.

sub _expand_name($)
{   my $node = shift;
    my $uri  = $node->namespaceURI || '';
    (length $uri ? "{$uri}" : '') . $node->localName;
}

sub collapse($$)
{   my ($self, $xml, $opts) = @_;
    $xml->isa('XML::LibXML::Element') or return;

    my (%data, $text);
    my $hooks = $self->{XCS_hooks};

    unless($opts->{noattr})
    {
      ATTR:
        foreach my $attr ($xml->attributes)
        {
            my $value;
            if($hooks && (my $hook = $hooks->{$attr->unique_key}))
            {   $value = $hook->($attr);
                defined $value or next ATTR;
            }
            else
            {   $value = $attr->value;
            }

            $value = $self->normalise_space($value)
                if !ref $value && $opts->{normalisespace}==2;

            my $name
              = !$attr->isa('XML::LibXML::Attr') ? $attr->nodeName
              : $opts->{nsexpand} ? _expand_name($attr)
              : $opts->{nsstrip}  ? $attr->localName
              :                     $attr->nodeName;

            _add_kv \%data, $name => $value, $opts;
        }
    }
    my $nr_attrs = keys %data;
    my $nr_elems = 0;

  CHILD:
    foreach my $child ($xml->childNodes)
    {
        if($child->isa('XML::LibXML::Text'))
        {   $text .= $child->data;
            next CHILD;
        }

        $child->isa('XML::LibXML::Element')
            or next CHILD;

        $nr_elems++;

        my $v;
        if($hooks && (my $hook = $hooks->{$child->unique_key}))
             { $v = $hook->($child) }
        else { $v = $self->collapse($child, $opts) }
        defined $v or next CHILD;

        my $name
          = $opts->{nsexpand} ? _expand_name($child)
          : $opts->{nsstrip}  ? $child->localName
          :                     $child->nodeName;

        _add_kv \%data, $name => $v, $opts;
    }

    $text = $self->normalise_space($text)
        if defined $text && $opts->{normalisespace}==2;

    return $opts->{forcecontent} ? { $opts->{contentkey} => $text } : $text
        if $nr_attrs+$nr_elems==0 && defined $text;

    $data{$opts->{contentkey}} = $text
        if defined $text && $nr_elems==0;

    # Roll up 'value' attributes (but only if no nested elements)

    if(keys %data==1)
    {   my ($k) = keys %data;
        return $data{$k} if $opts->{valueattrlist}{$k};
    }

    # Turn arrayrefs into hashrefs if key fields present

    if($opts->{keyattr})
    {   while(my ($key, $val) = each %data)
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
    {   my $newkey = $ka->{$name} or return $in;
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
    {   my $default_keys = "@default_attributes" eq "@$ka";

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
    {   next if !defined $v;
        next if ref $v eq 'HASH' && keys %$v == 1 && exists $v->{$contentkey};
        next if ref $v eq 'HASH' && !keys %$v;
        return \%out;
    }

    $out{$_} = $out{$_}{$contentkey} for keys %out;
    \%out;
}
  
1;

__END__

=chapter FUNCTIONS

The functions C<XMLin> (exported implictly) and C<xml_in>
(exported on request) simply call C<<XML::LibXML::Simple->new->XMLin() >>
with the provided parameters.

=chapter DETAILS

=section Parameter $xmldata

As first parameter to M<XMLin()> must provide the XML message to be
translated into a Perl structure.  Choose one of the following:

=over 4

=item A filename

If the filename contains no directory components, C<XMLin()> will look for the
file in each directory in the SearchPath (see OPTIONS below) and in the
current directory.  eg:

  $data = XMLin('/etc/params.xml', %options);

=item A dash  (-)

Parse from STDIN.

  $data = XMLin('-', %options);

=item undef

[deprecated]
If there is no XML specifier, C<XMLin()> will check the script directory and
each of the SearchPath directories for a file with the same name as the script
but with the extension '.xml'.  Note: if you wish to specify options, you
must specify the value 'undef'.  eg:

  $data = XMLin(undef, ForceArray => 1);

This feature is available for backwards compatibility with M<XML::Simple>,
but quite sensitive.  You can easily hit the wrong xml file as input.
Please do not use it: always use an explicit filename.

=item A string of XML

A string containing XML (recognised by the presence of '<' and '>' characters)
will be parsed directly.  eg:

  $data = XMLin('<opt username="bob" password="flurp" />', %options);

=item An IO::Handle object

In this case, XML::LibXML::Parser will read the XML data directly from
the provided file.

  # $fh = IO::File->new('/etc/params.xml') or die;
  open my $fh, '<:encoding(utf8)', '/etc/params.xml' or die;

  $data = XMLin($fh, %options);

=item An XML::LibXML::Document or ::Element

[Not available in XML::Simple] When you have a pre-parsed XML::LibXML
node, you can pass that.

=back

=section Parameter %options

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

  XMLin('<opt one="1">Two</opt>', ContentKey => 'text')

will parse to:

  { one => 1, text => 'Two' }

instead of:

  { one => 1, content => 'Two' }

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
     item => {
      one =>  'First'
      two =>  'Second'
    }
  }

rather than this (without the '-'):

  {
    item => {
      one => { content => 'First' }
      two => { content => 'Second' }
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

    { name => [ 'value' ] }

instead of this (the default):

    { name => 'value' }

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
    x => {         content => 'text1' },
    y => { a => 2, content => 'text2' }
  }

instead of:

  {
    x => 'text1',
    y => { 'a' => 2, 'content' => 'text2' }
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

=item HookNodes => CODE
Select document nodes to apply special tricks.
Introduced in [0.96], not available in XML::Simple.

When this option is provided, the CODE will be called once the XML DOM
tree is ready to get transformed into Perl.  Your CODE should return
either C<undef> (nothing to do) or a HASH which maps values of
unique_key (see M<XML::LibXML::Node> method C<unique_key> onto CODE
references to be called.

Once the translater from XML into Perl reaches a selected node, it will
call your routine specific for that node.  That triggering node found
is the only parameter.  When you return C<undef>, the node will not be
found in the final result.  You may return any data (even the node itself)
which will be included in the final result as is, under the name of the
original node.

Example:

   my $out = XMLin $file, HookNodes => \&protect_html;

   sub protect_html($$)
   {   # $obj is the instantated XML::Compile::Simple object
       # $xml is a XML::LibXML::Element to get transformed
       my ($obj, $xml) = @_;

       my %hooks;    # collects the table of hooks

       # do an xpath search for HTML
       my $xpc   = XML::LibXML::XPathContext->new($xml);
       my @nodes = $xpc->findNodes(...); #XXX
       @nodes or return undef;

       my $as_text = sub { $_[0]->toString(0) };  # as text
       #  $as_node = sub { $_[0] };               # as node
       #  $skip    = sub { undef };               # not at all

       # the same behavior for all xpath nodes, in this example
       $hook{$_->unique_key} = $as_text
           for @nodes;
 
       \%hook;
   }

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
      user => [
         { login    => 'grep',
           fullname => 'Gary R Epstein'
         },
         { login    => 'stty',
           fullname => 'Simon T Tyson'
         }
      ]
    }

If the option 'KeyAttr => "login"' were used to specify that the 'login'
attribute is a key, the same XML would parse to:

    {
      user => {
         stty => { fullname => 'Simon T Tyson' },
         grep => { fullname => 'Gary R Epstein' }
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
      user => {
         stty => {
            fullname => 'Simon T Tyson',
            login    => 'stty'
         },
         grep => {
            fullname => 'Gary R Epstein',
            login    => 'grep'
         }
      }
    }

The '+' indicates that the value of the key attribute should be copied
rather than moved to the folded hash key.

A '-' prefix would produce this result:

    {
      user => {
         stty => {
            fullname => 'Simon T Tyson',
            -login   => 'stty'
         },
         grep => {
            fullname => 'Gary R Epstein',
            -login    => 'grep'
         }
      }
    }

=item NoAttr => 1 I<# handy>

When used with C<XMLin()>, any attributes in the XML will be ignored.

=item NormaliseSpace => 0 | 1 | 2 I<# handy>

This option controls how whitespace in text content is handled.  Recognised
values for the option are:

=over 4
=item "0"
(default) whitespace is passed through unaltered (except of course for the
normalisation of whitespace in attribute values which is mandated by the XML
recommendation)

=item "1"
whitespace is normalised in any value used as a hash key (normalising means
removing leading and trailing whitespace and collapsing sequences of whitespace
characters to a single space)

=item "2"
whitespace is normalised in all text content

=back

Note: you can spell this option with a 'z' if that is more natural for you.

=item Parser => OBJECT

You may pass your own M<XML::LibXML> object, in stead of having one
created for you. This is useful when you need specific configuration
on that object (See M<XML::LibXML::Parser>) or have implemented your
own extension to that object.

The internally created parser object is configured in safe mode.
Read the M<XML::LibXML::Parser> manual about security issues with
certain parameter settings.  The default is unsafe!

=item ParserOpts => HASH|ARRAY

Pass parameters to the creation of a new internal parser object. You
can overrule the options which will create a safe parser. It may be more
readible to use the C<Parser> parameter.

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

=item ValueAttr => [ names ] I<# handy>

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

=item NsExpand => 0  I<advised>

When name-spaces are used, the default behavior is to include the
prefix in the key name.  However, this is very dangerous: the prefixes
can be changed without a change of the XML message meaning.  Therefore,
you can better use this C<NsExpand> option.  The downside, however, is
that the labels get very long.

Without this option:

  <record xmlns:x="http://xyz">
    <x:field1>42</x:field1>
  </record>
  <record xmlns:y="http://xyz">
    <y:field1>42</y:field1>
  </record>

translates into

  { 'x:field1' => 42 }
  { 'y:field1' => 42 }

but both source component have exactly the same meaning.  When C<NsExpand>
is used, the result is:

  { '{http://xyz}field1' => 42 }
  { '{http://xyz}field1' => 42 }

Of course, addressing these fields is more work.  It is advised to implement
it like this:

  my $ns = 'http://xyz';
  $data->{"{$ns}field1"};

=item NsStrip => 0 I<sloppy coding>

[not available in XML::Simple]
Namespaces are really important to avoid name collissions, but they are
a bit of a hassle.  To do it correctly, use option C<NsExpand>.  To do
it sloppy, use C<NsStrip>.  With this option set, the above example will
return

  { field1 => 42 }
  { field1 => 42 }

=back

=chapter EXAMPLES

When C<XMLin()> reads the following very simple piece of XML:

    <opt username="testuser" password="frodo"></opt>

it returns the following data structure:

    {
      username => 'testuser',
      password => 'frodo'
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
      person => [
        { email     => [ 'joe@smith.com', 'jsmith@yahoo.com' ],
          firstname => 'Joe',
          lastname  => 'Smith'
        },
        { email     => 'bob@smith.com',
          firstname => 'Bob',
          lastname  => 'Smith'
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
      person => {
         jbloggs => {
            firstname => 'Joe',
            lastname  => 'Bloggs'
         },
         tsmith  => {
            firstname => 'Tom',
            lastname  => 'Smith'
         },
         jsmith => {
            firstname => 'Joe',
            lastname => 'Smith'
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
      head => [ [ 'Col 1', 'Col 2', 'Col 3' ] ],
      data => [ [ 'R1C1', 'R1C2', 'R1C3' ],
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
    one => 'first',
    two => { attr => 'value', content => 'second' }
  }

Mixed content (elements which contain both text content and nested elements)
will be not be represented in a useful way - element order and significant
whitespace will be lost.  If you need to work with mixed content, then
XML::Simple is not the right tool for your job - check out the next section.

=section Differences to XML::Simple

In general, the output and the options are equivalent, although this
module has some differences with M<XML::Simple> to be aware of.

=over 4
=item only M<XMLin()> is supported
If you want to write XML then use a schema (for instance with
M<XML::Compile>). Do not attempt to create XML by hand!  If you still
think you need it, then have a look at XMLout() as implemented by
M<XML::Simple> or any of a zillion template systems.

=item no "variables" option
IMO, you should use a templating system if you want variables filled-in
in the input: it is not a task for this module.

=item empty elements are not removed
Being empty has a meaning which should not be ignored.

=item ForceArray options
There are a few small differences in the result of the C<forcearray> option,
because M<XML::Simple> seems to behave inconsequently.

=item hooks
XML::Simple does not support hooks.

=back

=chapter SEE ALSO

L<XML::Compile> for processing XML when a schema is available.  When you
have a schema, the data and structure of your message get validated.

L<XML::Simple>, the original implementation which interface is followed
as closely as possible.

=cut
