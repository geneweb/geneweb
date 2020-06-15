# Contributing

When contributing to this repository, please first discuss the change
you wish to make via issue, email, or any other method with the owners
of this repository before making a change.

## Pull request validation

When proposing a PR:

- Describe what problem it solves, what side effects come with it.
- Describe how to test the PR.
- Adding some screenshots will help.
- Add some documentation if relevant.
- Add some unit tests if relevant.
- Add some benchmarks if relevant.
- Add some comments around blocks/functions if relevant.

Some reasons why a PR could be refused:

- PR is not meeting one of the previous points.
- PR is not meeting
  [milestones](https://github.com/geneweb/geneweb/milestones) goals.
- PR is conflicting with another PR, and the latter is being prefered.
- PR slows down GeneWeb, or it obviously does to many
  computations for the task being accomplished. It needs to be
  optimized.
- PR is using copy-n-paste-programming. It needs to be factorized.
- PR contains commented code: remove it.
- PR adds new features or changes the behavior of GeneWeb without
  having be approved by the current project owners first.
- PR is too big and needs to be splitted in many smaller ones.
- PR contains "indentation fixes".

If a PR stays in a stale/WIP/POC state for too long, it may be closed
at any time.
