CREATE TABLE IF NOT EXISTS project
(
    id integer primary key,
    name text,
    description text,
    link text,
    logo text,
    img text,
    fk_lang integer,
    fk_license integer
);

CREATE TABLE IF NOT EXISTS license
(
    id integer primary key,
    name text,
    text text
);

CREATE TABLE IF NOT EXISTS coding
(
    id integer primary key,
    name text
);

CREATE TABLE IF NOT EXISTS project_coding
(
    fk_project_id integer,
    fk_coding_id integer
);

CREATE TABLE IF NOT EXISTS project_license
(
    fk_project_id integer,
    fk_license_id integer
);
