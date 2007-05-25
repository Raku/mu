from buildbot.steps.shell import ShellCommand

class SmokeTests(ShellCommand):
  name = "smoke tests"
  description = ["running smoke tests"]
  descriptionDone = [name]

class BundleOSX(ShellCommand):
  name = "bundle OS X"
  description = ["bundling OS X"]
  descriptionDone = [name]
