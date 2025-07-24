#!/bin/bash

# Create temporary log file
log_file=$(mktemp /tmp/check-ghq-XXXXXX.txt)
echo "GHQ Repository Check - $(date)" > "$log_file"
echo "================================" >> "$log_file"

# Colors
YELLOW='\033[1;33m'
CYAN='\033[1;36m'
GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m' # No Color

# Main loop
for repo in $(ghq list); do
    repo_path="$(ghq root)/$(echo "$repo" | tr ':' '/')"
    cd "$repo_path" || continue

    branch=$(git branch --show-current 2> /dev/null)
    changes=$(git status --porcelain 2> /dev/null)
    unpushed=$(git log --branches --not --remotes --oneline --decorate-refs=refs/heads --decorate=short 2> /dev/null)

    if [[ -n "$changes" || -n "$unpushed" ]]; then
        echo -e "\n${YELLOW}$repo${NC} ${CYAN}[$branch]${NC}"
        echo -e "\n$repo [$branch]" >> "$log_file"

        if [[ -n "$changes" ]]; then
            echo "  📝 Changes:"
            echo "$changes" | sed 's/^/    /'
            echo "  Changes:" >> "$log_file"
            echo "$changes" | sed 's/^/    /' >> "$log_file"
        fi

        if [[ -n "$unpushed" ]]; then
            echo "  ⬆️  Unpushed commits:"
            echo "$unpushed" | sed 's/^/    /'
            echo "  Unpushed commits:" >> "$log_file"
            echo "$unpushed" | sed 's/^/    /' >> "$log_file"
        fi

        echo -e "\n  Actions: [c]ommit, [p]ush, [b]oth, [s]kip, [q]uit?"
        read -r -p "  > " action

        case $action in
            c)
                if [[ -n "$changes" ]]; then
                    read -r -p "  Commit message: " msg
                    git add -A && git commit -m "$msg"
                    echo "  ✅ Committed: $msg" | tee -a "$log_file"
                else
                    echo "  ⚠️  No changes to commit" | tee -a "$log_file"
                fi
                ;;
            p)
                if [[ -n "$unpushed" ]]; then
                    # Check if remote is set
                    if ! git remote -v | grep -q "origin"; then
                        echo "  ⚠️  No remote repository set" | tee -a "$log_file"
                        read -r -p "  Create GitHub repository? [y/N]: " create_repo
                        if [[ "$create_repo" =~ ^[Yy]$ ]]; then
                            # Extract repo name from current directory
                            repo_name=$(basename "$(dirname "$PWD")")/$(basename "$PWD")
                            gh repo create "$repo_name" --source=. --private
                            echo "  ✅ Created GitHub repository: $repo_name" | tee -a "$log_file"
                        else
                            echo "  ⏭️  Skipped repository creation" | tee -a "$log_file"
                            continue
                        fi
                    fi
                    git push
                    echo "  ✅ Pushed to remote" | tee -a "$log_file"
                else
                    echo "  ⚠️  Nothing to push" | tee -a "$log_file"
                fi
                ;;
            b)
                if [[ -n "$changes" ]]; then
                    read -r -p "  Commit message: " msg
                    git add -A && git commit -m "$msg"
                    echo "  ✅ Committed: $msg" | tee -a "$log_file"
                fi
                # Check if remote is set
                if ! git remote -v | grep -q "origin"; then
                    echo "  ⚠️  No remote repository set" | tee -a "$log_file"
                    read -r -p "  Create GitHub repository? [y/N]: " create_repo
                    if [[ "$create_repo" =~ ^[Yy]$ ]]; then
                        # Extract repo name from current directory
                        repo_name=$(basename "$(dirname "$PWD")")/$(basename "$PWD")
                        gh repo create "$repo_name" --source=. --private
                        echo "  ✅ Created GitHub repository: $repo_name" | tee -a "$log_file"
                    else
                        echo "  ⏭️  Skipped repository creation" | tee -a "$log_file"
                        continue
                    fi
                fi
                git push
                echo "  ✅ Pushed to remote" | tee -a "$log_file"
                ;;
            s)
                echo "  ⏭️  Skipped" | tee -a "$log_file"
                ;;
            q)
                echo -e "\n🛑 Quit by user" | tee -a "$log_file"
                echo -e "\n📄 Log saved to: $log_file"
                exit 0
                ;;
            *)
                echo "  ❓ Invalid option, skipping" | tee -a "$log_file"
                ;;
        esac
    fi
done

echo -e "\n✨ All repositories checked!" | tee -a "$log_file"
echo "📄 Log saved to: $log_file"
