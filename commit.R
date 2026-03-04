system("git pull")
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(
  '/opt/homebrew/bin/rsync -av --delete --iconv=utf-8-mac,utf-8 --exclude=".quarto" --exclude=".git" --exclude="Archive" \\
  "/Users/benstanley/Positron/Grants/" \\
  "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Grants/"'
)
