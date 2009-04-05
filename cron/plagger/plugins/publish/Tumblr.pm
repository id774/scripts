package Plagger::Plugin::Publish::Tumblr;
use strict;
use base qw( Plagger::Plugin );

use Encode;
use Time::HiRes qw(sleep);
use WWW::Tumblr;

sub register {
    my($self, $context) = @_;
    $context->register_hook(
        $self,
        'publish.entry' => \&publish_entry,
        'plugin.init'   => \&initialize,
    );
}

sub initialize {
    my($self, $context) = @_;

    $self->{tumblr} = WWW::Tumblr->new;
    $self->{tumblr}->email($self->conf->{username});
    $self->{tumblr}->password($self->conf->{password});
}

sub publish_entry {
    my($self, $context, $args) = @_;
    
    my $title = $args->{entry}->{title};
    $title = encode_utf8($title);
    my $body = $args->{entry}->{body};
    $body = encode_utf8($body);
    my $link = $args->{entry}->{link};
    
    my $type = $self->conf->{type} || 'regular';
    
    $context->log(info => "Tumblr($type) posting '$title'");
    if($type eq 'text'){
        my $post = $body . "<div><a href=\"" . $link . "\">" . $title . "</a></div>";
        $self->{tumblr}->write(
            type => 'regular',
            title => $title,
            body => $post,
            );
    }
    elsif($type eq 'quote'){
        my $source = "<a href=\"" . $link . "\">" . $title . "</a>";
        $self->{tumblr}->write(
            type => 'quote',
            quote => $body,
            source => $source,
            );
    }
    elsif($type eq 'link'){
        $self->{tumblr}->write(
            type => 'link',
            name => $title,
            url => $link,
            description => $body,
            );
    }
    
    my $sleeping_time = $self->conf->{interval} || 5;
    $context->log(info => "sleep $sleeping_time.");
    sleep( $sleeping_time );
}

1;

__END__

=head1 NAME

Plagger::Plugin::Publish::Tumblr - Post to Tumblr

=head1 SYNOPSIS

  - module: Publish::Tumblr
    config:
      username: your-email
      password: your-password
      type: post type(text, quote, or link)
      interval: 2

=head1 DESCRIPTION

This plugin automatically posts feed updates to Tumblr
L<http://www.tumblr.com/>.

You can use 3 types of Post "text", "quote", and "link".
- text
    - title
    - body(added link to the url at the end)
- quote
    - quote(body)
    - source(title linked to the url)
- link
    - name(title)
    - url
    - description(body)

=head1 AUTHOR

riywo

=head1 SEE ALSO

L<Plagger>

=cut
