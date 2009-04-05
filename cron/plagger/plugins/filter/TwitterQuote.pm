package Plagger::Plugin::Filter::TwitterQuote;
use strict;
use base qw( Plagger::Plugin );

use Encode;

sub register {
    my ($self, $context) = @_;
    $context->register_hook(
        $self,
        'update.entry.fixup' => \&filter,
    );
}

sub filter {
    my ($self, $context, $args) = @_;
    
    my $body = $args->{entry}->{body};
    $body =~ s/\r|\n//g;
    $body =~ /^(.+?): (.+)$/o;
    my $id = $1;
    my $quote = encode_utf8($2);
    my $title = "Twitter / " . $id;
    
    $args->{entry}->title($title);
    $args->{entry}->body($quote);
}

1;

__END__

=head1 NAME

Plagger::Plugin::Filter::TwitterQuote - Making Twitter feed to Tumblr Quote.

=head1 SYNOPSIS

  - module: Filter::TwitterQuote

=head1 DESCRIPTION

This plugin makes twitter feed to tumblr quote.
L<http://www.tumblr.com/>.

=head1 AUTHOR

riywo

=head1 SEE ALSO

L<Plagger>

=cut
