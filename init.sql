CREATE TABLE repos (
    id      INTEGER  PRIMARY KEY,
    url    TEXT     NOT NULL,
    name    TEXT     NOT NULL
);

CREATE TABLE issues (
    id           INTEGER  PRIMARY KEY,
    url         TEXT     NOT NULL,
    name         TEXT     NOT NULL,
    repo_id      INTEGER  REFERENCES repos(id) NOT NULL,
    description  TEXT
);

-- CREATE TABLE repos_issues (
--     repo_id   INTEGER  REFERENCES repos NOT NULL,
--     issue_id  INTEGER  REFERENCES issues NOT NULL
-- );
