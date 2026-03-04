system("git pull")
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(
  '/opt/homebrew/bin/rsync -av --delete --modify-window=1 --exclude=".git" --exclude="Archive" \\
  --protect-args \\
  "/Users/benstanley/Positron/Grants/" \\
  "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Grants/"'
)
