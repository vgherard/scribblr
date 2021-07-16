post_github_issue <- function(title, body) {

	ght <- gh::gh_tree_remote()
	title <- URLencode(title)
	body <- URLencode(body)

	url <- sprintf("https://github.com/%s/%s/issues/new?title=%s&body=%s",
				   ght[["username"]], ght[["repo"]], title, body)
	browseURL(url)

}
