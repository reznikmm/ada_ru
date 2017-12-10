create table posts (
  id      varchar primary key,
  author  varchar not null,
  sent    varchar not null,
  parent  varchar,
  subject varchar not null,
  text    varchar);

create table forums (
  id       integer primary key,
  sort_key integer not null,
  subject  varchar not null);

create table topics (
  starter  varchar primary key,
  forum    integer not null references forums,
  subject  varchar not null);

create table users (
  nickname varchar primary key,
  name     varchar not null,
  avatar   varchar);

create table emails (
  email    varchar primary key,
  nickname varchar not null references users,
  main     char(1));
