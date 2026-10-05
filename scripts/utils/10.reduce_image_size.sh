#!/usr/bin/env bash

# Exit immediately if a command exits with a non-zero status (-e),
# treat unset variables as an error (-u), and fail on pipe errors (-o pipefail).
set -euo pipefail

# PATH_COPY="output/images/main_page_image output/images/main_page_image2"
# PATH_PHOTOS="$HOME/Github_proj/Guide_biodiv_QC/output/images/main_page_image2/"

# Copier les données (doit supprimer main_page_image2 avant)
cp -R PATH_COPY

find $PATH_PHOTOS -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) -exec magick mogrify -resize 200 {} +

# ------------------------------------------------------------------------------
# Usage Function
# ------------------------------------------------------------------------------
usage() {
  cat <<EOF
Usage: $(basename "$0") <source_directory_or_file> <target_directory>

Description:
  Copier les données d'un dossier source vers un dossier de destination :  
  (e.g., main_page_image/* --> main_page_image2/*)
  Réduire la taille des photos au format PNG et JPG, 
  écrase les fichiers originaux dans le dossier de copie). 
  L'argument $(resize 200) compromis de résolution et d'espace de stockage.

Arguments:
  source_path  Chemin d'accès du dossier source à copier.
  target_path  Chemin d'accès du dossier de destination pour faire la copie des 
               images et réduire leur taille.

Example:
  $0 "output/images/main_page_image" "$HOME/Github_proj/Guide_biodiv_QC/output/images/main_page_image2/"
EOF
  exit 1
}

# ------------------------------------------------------------------------------
# Input Validation & Dependency Checks
# ------------------------------------------------------------------------------
# Ensure exactly two arguments are provided
if [[ $# -ne 2 ]]; then
  echo "Error: Exactly 2 path arguments are required." >&2
  usage
fi

SRC_PATH="$1"
TARGET_PATH="$2"

# Check if source path exists
if [[ ! -e "$SRC_PATH" ]]; then
  echo "Error: Source path '$SRC_PATH' does not exist." >&2
  exit 1
fi

# Check if ImageMagick 'magick' tool is installed
if ! command -v magick &>/dev/null; then
  echo "Error: 'magick' (ImageMagick) is not installed or not in PATH." >&2
  exit 1
fi

# ------------------------------------------------------------------------------
# Script Execution
# ------------------------------------------------------------------------------
echo "==> Preparing target directory..."
# Safely remove existing target directory if present to avoid dirty copies
if [[ -d "$TARGET_PATH" ]]; then
  rm -rf "$TARGET_PATH"
fi

# Ensure target parent directory exists
mkdir -p "$(dirname "$TARGET_PATH")"

echo "==> Copie de '$SRC_PATH' vers '$TARGET_PATH'..."
# Copy recursively; -a preserves timestamps, permissions, and links
cp -a "$SRC_PATH" "$TARGET_PATH"

echo "==> Réduction de la taille des images dans '$TARGET_PATH'..."
# Use find with -exec to safely handle spaces in paths
# Resize images to max width 200px maintaining aspect ratio
find "$TARGET_PATH" -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) \
  -exec magick mogrify -resize 200 {} +

echo "==> Copie et réduction de la taille terminée."
