from dao_bias.jobs import author_votes as author_votes_job, human_labels as human_labels_job, cumulative as vote_cumulative_job, votes as vote_expansion_job


JOB_REGISTRY = {
    "author_votes": author_votes_job,
    "human_labels": human_labels_job,
    "vote_cumulative": vote_cumulative_job,
    "vote_expansion": vote_expansion_job,
}


def get_job(job_name):
    if job_name not in JOB_REGISTRY:
        known = ", ".join(sorted(JOB_REGISTRY))
        raise KeyError(f"Unknown job '{job_name}'. Known jobs: {known}")
    return JOB_REGISTRY[job_name]
