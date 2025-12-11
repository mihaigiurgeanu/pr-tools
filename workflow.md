# Git + pr-tools Workflow

This document summarizes a best-practices workflow for using **pr-tools** with a decentralized, Slack-driven pull request review process.

---

## 1. Prepare Your Git Repository

**Initial Setup (once per developer):**
```sh
git clone [REPO_URL] project-name
cd project-name
git config user.name "Your Full Name"
git config user.email "you@example.com"
# Optional but useful:
git config alias.st status
git config alias.co checkout
git config alias.br branch
git config alias.up "pull --rebase"
```

**Daily Start:**
```sh
git checkout master
git pull
```

---

## 2. Start a Feature

**Create a Feature Branch:**
```sh
git checkout master && git pull
git checkout -b feature/descriptive-name
# Good branch names: feature/user-auth, fix/test-fix, bugfix/logo-fix, hotfix/security-issue
```

**Work and Commit Regularly:**
```sh
# Edit, add, and commit files as you develop
git add .
git commit -m "Describe change clearly"
git push -u origin feature/descriptive-name    # First push
git push                                       # Subsequent pushes
```
Tip: Push at least daily for team visibility.

**Keep Feature Branch Updated:**
```sh
git checkout master && git pull
git checkout feature/descriptive-name
git rebase master
git push origin --force-with-lease
```
Checklist:
- [ ] Branch from updated master
- [ ] Clear, conventional name
- [ ] Frequent, descriptive commits
- [ ] Frequently pushed to remote
- [ ] Regularly rebased on master

---

## 3. Prepare PR for Review

**Finalize Branch:**
```sh
git status       # Should be clean
git push
git checkout master && git pull
git checkout feature/descriptive-name
git rebase master
# Optional: Condense history
git rebase -i master
git push --force-with-lease
```

**Initiate Pull Request Workflow (Slack):**
```sh
pr-snapshot         # Generates .pr-drafts markdown for this PR
pr-send             # Posts PR summary to Slack
```

---

## 4. Review the PR

**Reviewing Code:**
```sh
git fetch
git checkout feature/descriptive-name
git pull
git log --oneline master..feature/descriptive-name
git diff master..feature/descriptive-name
pr-review start
pr-review files
pr-review open         # Inspect changes, add freeform comments
pr-review next         # Move to next file
pr-review previous     # Move to previous file
pr-review comment --file FILE --line N --text "Comment"    # Add comments directly
pr-review comments
pr-view diff --full
pr-review resolve --id <comment-id>
```

**Adding Comments in Editor:**
When using `pr-review open`, `next`, or `previous`, the file opens in your configured editor. To add a comment, simply insert new lines with your feedback anywhere in the file. When you save and exit, the tool detects these added lines as comments attached to the surrounding code.

**Test the Feature:**
```sh
pytest tests      # Or run your test suite
# Test the application and new feature manually
```

**Finish Review:**
```sh
pr-review end
pr-review send    # Posts review comments to Slack
```
Approval: Inform the author and team on Slack when approving!

---

## 5. Fix Issues Raised in Review

**Update Feature Branch:**
```sh
git checkout master && git pull
git checkout feature/descriptive-name
git rebase master
pr-fix start      # Paste Slack review message
```

**Triage and Fix:**
```sh
pr-fix files
pr-fix comments
pr-fix open       # Work through files with comments, answer and set status (solved/not-solved/will-not-solve)
pr-fix next
pr-fix previous
pr-fix resolve --id ID --status STATUS [--answer "Explanation"]
```

**Answering Comments in Editor:**
When using `pr-fix open`, `next`, or `previous`, the file opens in your editor with review comments inserted.
To change the status, edit the `[status: ...]` tag in the header line (ensure it remains lowercase).
To provide an answer, you can either:
1. Add `[answer: Your answer]` to the header line (must end with `]`).
2. Add a line containing `[answer:` in the comment body, followed by your answer on the subsequent lines.

**Prep for Another Review:**
```sh
git status    # Should be clean
git push
git checkout master && git pull
git checkout feature/descriptive-name
git rebase master
git rebase -i master   # (Optional: squash commits)
git push --force-with-lease
pr-fix end
```

**Re-notify Slack:**
```sh
pr-fix send     # Post fix summary to Slack
```

---

## 6. Review the Fixes

**Update Local Branch:**
```sh
git fetch
git checkout feature/descriptive-name
git pull
```

**Import Fix Explanations:**
If the developer sent a fix summary to Slack, you can import their answers and status updates into your review session:
```sh
pr-review start           # Resume your review session
pr-review import-answers  # Paste the fix summary from Slack
```

**Verify Fixes:**
Check if the reported issues are resolved:
```sh
pr-review comments        # See updated statuses and answers
pr-view diff              # Check the new code changes
pr-review files
pr-review open            # Inspect changes, add freeform comments
pr-review next            # Move to next file
pr-review previous        # Move to previous file
pr-review comment --file FILE --line N --text "Comment"    # Add comments directly
```

**Resolve Comments:**
If a comment is satisfactorily addressed:
```sh
pr-review resolve --id ID
```

**Finalize:**
End the review session:
```sh
pr-review end
```

If all comments are resolved:
- Notify the author on Slack (e.g., "Approved!").

If issues remain:
- Run `pr-review send` to post the remaining feedback to Slack.

---

## 7. Merge Pull Request

**Gather Approvals:**
When a PR is approved by a reviewer, the reviewer will notify you on Slack. You should record
his approval:

```sh
pr-track approve --by [ReviewerName]
pr-track status [BRANCH]
pr-track list
```

**Verify Prereqs:**
- [ ] Enough approvals received (2 for features, 1 for bugs)
- [ ] Feedback addressed; no unresolved comments
- [ ] Branch rebased, no conflicts
- [ ] All tests pass

**Merge:**
```sh
git checkout master && git pull
git fetch
git log --oneline master..origin/feature/descriptive-name
git checkout feature/descriptive-name && git pull
git rebase master
pr-merge          # Use appropriate strategy if needed
git push
```

**Clean up:**
```sh
git push origin --delete feature/descriptive-name
git branch -d feature/descriptive-name
```

---

## Quick Reference: Recovery

**Abort Merge:**
```sh
git merge --abort
git reset --hard HEAD
```
**Undo Last Commit (Not Yet Pushed):**
```sh
git reset HEAD~1         # Unstage, keep local changes
git reset --soft HEAD~1  # Keep changes staged
git reset --hard HEAD~1  # Discard changes
```
**Push Fixes After Review Feedback:**
```sh
git add .
git commit -m "Address feedback"
git push origin feature/branch-name
```

---

## Team Rules Summary

1. Never commit directly to main—always use feature branches.
2. All features must be reviewed—no self-merging.
3. Keep commits focused and descriptive.
4. Write clear, meaningful commit messages.
5. Test thoroughly before requesting review.
6. Address all feedback before merging.
7. Delete merged branches locally and remotely.
8. Review respectfully and constructively.

---
