#!/bin/sh

_app_name='my-app'
_app_id="org.example.${_app_name}"
_flatpak_yaml="${_app_id}.yml"
_build_dir='.build-dir'
_local_repo='.local-repo'

# Installing build dependencies for Flatpak
flatpak install flathub org.freedesktop.Platform//25.08 org.freedesktop.Sdk//25.08

# Invoking the Flatpak builder to build the package
flatpak-builder "${_build_dir}" "${_flatpak_yaml}" --force-clean

# Run the application
flatpak-builder --run "${_build_dir}" "${_app_name}"

# If you want to install it locally
#flatpak-builder --user --install --force-clean "${_build_dir}" "${_flatpak_yaml}"

# Exporting the Flatpak to a file that can be moved around
flatpak-builder --force-clean --repo "${_local_repo}" "${_build_dir}" "${_flatpak_yaml}"
flatpak build-bundle "${_local_repo}" "${_app_name}.flatpak" "${_app_id}"

# Importing the Flatpak
# flatpak install --user "${_app_name}.flatpak"
