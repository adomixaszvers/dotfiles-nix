{
  curl,
  jq,
  lib,
  writers,
}:
writers.writeDashBin "jj-jr" ''
  if [ -z "$1" ] || [ -z "$JIRA_TOKEN" ] || [ -z "$JIRA_PROJECT" ] || [ -z "$JIRA_HOST" ]; then
    exit 1
  fi

  TICKET_NUMBER="$JIRA_PROJECT-$1"

  SUMMARY="$(${lib.getExe curl} -q --fail -H "Content-Type: application/json"  -H "Authorization: Bearer $JIRA_TOKEN" "https://$JIRA_HOST/rest/api/2/issue/$TICKET_NUMBER?fields=summary"| ${lib.getExe jq} --raw-output .fields.summary)"

  jj git fetch
  jj new 'trunk()'
  jj desc -m "$TICKET_NUMBER: $SUMMARY"
  jj bookmark set "feature/$TICKET_NUMBER"
''
