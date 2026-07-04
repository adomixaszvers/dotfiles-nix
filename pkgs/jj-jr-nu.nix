{ writers }:
writers.writeNuBin "jj-jr" /* nu */ ''
  def main [ticket_number: int] {
    if not ('JIRA_TOKEN' in $env) or not ('JIRA_PROJECT' in $env) or not ('JIRA_HOST' in $env) {
        exit 1
    }

    let ticket_number = $"($env.JIRA_PROJECT)-($ticket_number)"

    let summary = http get --headers {accept: 'application/json', authorization: $"Bearer ($env.JIRA_TOKEN)"} $"https://($env.JIRA_HOST)/rest/api/2/issue/($ticket_number)?fields=summary"| get fields.summary

    jj git fetch
    jj new 'trunk()'
    jj desc -m $"($ticket_number): ($summary)"
    jj bookmark set $"feature/($ticket_number)"
  }
''
