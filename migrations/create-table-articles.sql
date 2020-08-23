CREATE TABLE IF NOT EXISTS articles (
    id         SERIAL NOT NULL PRIMARY KEY,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    deleted_at TIMESTAMP DEFAULT NULL,

    title      VARCHAR(128),
    slug       VARCHAR(128),
    content    TEXT
);

CREATE INDEX IF NOT EXISTS idx_articles_title ON articles (
    title ASC NULLS LAST
);

CREATE INDEX IF NOT EXISTS idx_articles_deleted_at ON articles (
    deleted_at ASC NULLS LAST
);
