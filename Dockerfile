FROM ubuntu:xenial

COPY docker /var/dotfiles-setup
RUN /var/dotfiles-setup/create-user.sh && rm -rf /var/dotfiles-setup
