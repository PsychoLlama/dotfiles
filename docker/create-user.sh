#!/usr/bin/env bash
set -e

apt-get update
apt-get install -y git sudo

adduser --disabled-password --gecos overlord overlord
usermod -aG sudo overlord

# Allow sudo without a password.
echo 'overlord ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers.d/overlord

su -c /var/dotfiles-setup/install-dotfiles.sh overlord
