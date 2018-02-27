#!/usr/bin/env bash
set -e

apt-get update
apt-get install -y git sudo

adduser --disabled-password --gecos overlord overlord
usermod -aG sudo overlord

# Allow passwordless `sudo`.
echo 'overlord ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers.d/overlord

# Allow passwordless `chsh`.
echo 'auth sufficient pam_wheel.so trust group=sudo' >> /etc/pam.d/chsh

su -c /var/dotfiles-setup/install-dotfiles.sh overlord
