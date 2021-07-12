post_github_issue <- function(title, body) {

	ght <- gh::gh_tree_remote()

	ep <- sprintf("POST /repos/%s/%s/issues", ght[["username"]], ght[["repo"]])
	gh::gh(
		endpoint = ep,
		title = "Issue title",
		body = "Some text"
	)

}
