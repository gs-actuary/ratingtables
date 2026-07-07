# Official base-R package demo for ratingtables.
# Run after installation with: demo("rating_example", package = "ratingtables")

ex <- example_rating_plan()
plan <- ex$plan
policies <- ex$policies

print(plan)

cat("\nRated policies:\n")
rated <- rate_policies(policies, plan)
print(rated)

cat("\nRated policies with caps:\n")
capped <- apply_caps(rated, coverages = c("BI", "PD"))
print(capped)

cat("\nTerm trace:\n")
result <- rate_policies_with_trace(policies, plan, trace_detail = "matches")
print(result$term_trace)

cat("\nOne-policy explanation:\n")
print(explain_rating(policies[1, ], plan, coverage = "BI"))

cat("\nWide factor view:\n")
print(trace_to_wide_factors(result$term_trace))
