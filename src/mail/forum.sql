create table posts (
  id      varchar primary key,
  author  varchar not null,
  sent    varchar not null,
  parent  varchar,
  subject varchar not null);

create table post_lines (
  post    varchar not null references posts,
  pos     integer not null,
  quote   integer not null default 0,
  text    varchar,
  primary key (post, pos));

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
  avatar   varchar,
  created timestamp default current_timestamp);

create table emails (
  email    varchar primary key,
  nickname varchar not null references users,
  main     char(1));

INSERT INTO forums VALUES(1,10,'Обсуждение языка Ада');

create table tg_books (
  id        varchar primary key,
  file_name varchar not null,
  title     varchar,
  caption   varchar,
  descr     varchar);

create table game_stations (
  station varchar not null primary key,
  name    varchar not null
);

insert into game_stations values ('library','Elementary');

create table game_missions (
  mission varchar not null primary key,
  station varchar not null references game_stations,
  name    varchar not null,
  points  integer not null
);

insert into game_missions (mission, station, name, points) values
('multiply-intro', 'library', 'Multiply (Intro)', 5);

insert into game_missions (mission, station, name, points) values
('easy-unpack', 'library', 'Easy Unpack', 5);

insert into game_missions (mission, station, name, points) values
('first-word-simplified', 'library', 'First Word (simplified)', 5);

insert into game_missions (mission, station, name, points) values
('acceptable-password-i', 'library', 'Acceptable Password I', 5);

insert into game_missions (mission, station, name, points) values
('number-length', 'library', 'Number Length', 5);

insert into game_missions (mission, station, name, points) values
('end-zeros', 'library', 'End Zeros', 5);

insert into game_missions (mission, station, name, points) values
('backward-string', 'library', 'Backward String', 5);

create table game_mission_votes (
  nickname varchar not null references users,
  mission  varchar not null references game_missions,
  vote     integer not null,
  primary key (nickname, mission)
);

create table solved_missions (
  nickname varchar not null references users,
  mission  varchar not null references game_missions,
  solved   timestamp not null default current_timestamp,
  primary key (nickname, mission)
);

create table solution_texts (
  nickname varchar not null references users,
  mission  varchar not null references game_missions,
  line     integer not null,
  text     varchar not null,
  primary key (nickname, mission, line)
);
