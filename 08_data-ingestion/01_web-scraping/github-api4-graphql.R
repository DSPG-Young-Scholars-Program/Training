# Install the ghql package if necessary
devtools::install_github("ropensci/ghql")

# Load libraries
library("ghql")
library("jsonlite")
library("httr")

# Get a Graphql Oauth Toekn (for API4) from GitHub
# go here to get your OATH token: https://github.com/settings/tokens
# make sure to copy the token once created. You can see it only once after creation.
# put the token in your .Renviron file in your home directory
# so you should have a line in your .Renviron file that looks like GITHUB_GRAPHQL_TOKEN='e03cxxxxa86c2d3axxxxxxx4ef68d35ce6dab922'

# Read token from .Renviron file
token <- Sys.getenv("GITHUB_GRAPHQL_TOKEN")

# Create a new GraphQL client
cli <- GraphqlClient$new(
  url = "https://api.github.com/graphql",
  headers = add_headers(Authorization = paste0("Bearer ", token))
)

# Load the github schema (I think you have to do this)
cli$load_schema()

# Create a new query
qry <- Query$new()

# Specify the query
# HIGHLY SUGGESTED - Go here to develop your query: https://developer.github.com/v4/explorer/
qry$query('getmydata2', '{
  repositoryOwner(login: "dads2busy") {
    repositories(first: 3, orderBy: {field: PUSHED_AT, direction: DESC}, isFork: false) {
      edges {
        node {
          nameWithOwner
          createdAt
          updatedAt
          description
          primaryLanguage {
            name
            color
          }
          stargazers {
            totalCount
          }
          isFork
          isArchived
          isPrivate
          ref(qualifiedName: "master") {
            target {
              ... on Commit {
                history(first: 3) {
                  edges {
                    node {
                      committedDate
                    }
                  }
                }
              }
            }
          }
        }
      }
      edges {
        cursor
      }
      pageInfo {
        hasNextPage
      }
    }
  }
}')

# Execute the query
resp <- cli$exec(qry$queries$getmydata2)

# Take a look at what got returned
jqr::jq(resp)

# Convert to an R object (list of lists and data.frames
fj <- jsonlite::fromJSON(resp)

# Get a data.frame of the repository information
fj$data$repositoryOwner$repositories$edges$node



# Load libraries
library("ghql")
library("jsonlite")
library("httr")

# Build function
get_additions_deletions <- function(repo_owner = "dads2busy", repo_name = "dataplumbr", commit_sha = "11d0c6f6cd0b96d436a4cefcea32da16699ddc43") {
  # Read GRAPHQL token from .Renviron file
  token <- Sys.getenv("GITHUB_GRAPHQL_TOKEN")
  
  # Create a new GraphQL client
  cli <- GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = add_headers(Authorization = paste0("Bearer ", token))
  )
  
  # Create a new query
  qry <- Query$new()
  
  # Build the query
  qry$query("add_del_query", paste0(
          "{
              repository(owner: \"", repo_owner, "\" name: \"", repo_name, "\") {
                object(oid: \"", commit_sha, "\") {
                  ... on Commit {
                    additions
                    deletions
                  }
                }
              }
            }"
          ))
  
  # Execute the query
  resp <- cli$exec(qry$queries$add_del_query)
  
  # Convert to an R object
  fj <- jsonlite::fromJSON(resp)
  
  # Return a list of the additions and deletions
  fj$data$repository$object
}

# Run function
get_additions_deletions()





