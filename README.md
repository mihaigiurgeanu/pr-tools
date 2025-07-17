# PR Tools

A set of command-line tools for managing pull requests in a Git repository. These tools help with creating PR snapshots, sending notifications to Slack, tracking approvals, merging PRs, reviewing changes, and viewing annotated diffs.

## Features

- **pr-snapshot**: Generate a Markdown snapshot of the PR including commits and diff summary.
- **pr-send**: Send the PR snapshot to a Slack channel for review.
- **pr-track**: Track PR approvals and status.
- **pr-merge**: Merge approved PRs with various strategies and update changelog.
- **pr-review**: Interactive code review tool with comment management.
- **pr-view**: View annotated diffs with review comments.

## Installation

This project is built with Haskell and Cabal. To install:

1. Ensure you have [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed.
2. Clone the repository:
   ```
   git clone <repository-url>
   cd pr-tools
   ```
3. Build and install the executables:
   ```
   cabal update
   cabal install
   ```

This will install the binaries in `~/.cabal/bin` (or equivalent). Add this directory to your PATH if necessary.

## Configuration

### Base Branch and Slack Webhook

Create a `.pr-tools.yaml` file in the project root with:
```yaml
base-branch: main  # or your default base branch
slack-webhook: "https://hooks.slack.com/services/T00000000/B00000000/XXXXXXXXXXXXXXXXXXXXXXXX"  # optional Slack webhook URL
```

### Slack Integration

For `pr-send` and `pr-merge` to post notifications to Slack, add the `slack-webhook` to your `.pr-tools.yaml` file.

- Generate a webhook URL in your Slack workspace: Go to [Slack Apps](https://api.slack.com/apps), create an app, and enable Incoming Webhooks.

If not set, `pr-send` will fail with an error, and `pr-merge` will skip the notification.

## Usage

### pr-snapshot

Generate a PR snapshot:
```
pr-snapshot [BRANCH] [--base-branch BASE]
```

### pr-send

Send the snapshot to Slack:
```
pr-send [BRANCH]
```

### pr-track

Manage PR tracking:
```
pr-track approve <BRANCH> [--by <NAME>]
pr-track status <BRANCH>
pr-track list
```

### pr-merge

Merge a PR:
```
pr-merge <BRANCH> [--strategy {fast-forward|squash|rebase}] [--base-branch BASE]
```

### pr-review

Manage code reviews:
```
pr-review start
pr-review next
pr-review previous
pr-review open
pr-review files
pr-review changes
pr-review comment --file FILE --line LINE --text "COMMENT"
pr-review resolve --id ID
pr-review end
pr-review list
```

### pr-view

View annotated diff:
```
pr-view <BRANCH> [--base-branch BASE]
```

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## License

This project is licensed under the BSD-3-Clause License.
