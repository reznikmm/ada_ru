insert into posts (id,author,sent,subject) values ('x','x','x','x');
update post_lines set post='x' where post='';
update posts set id='<@>' where id='';
update post_lines set post='<@>' where post='x';
delete from posts where id='x';

update posts set subject='10 причин выучить Аду'
where id ='<99C9C25E724C45449F8611027649216B@liubouhome>';

update posts set subject='Статья в еженедельнике КГ'
where id='<3CA23C3C623945C98EF6487554AA4442@microsofd32266>';

update posts set parent='<577E0D81.50707@...>'
where id='<1504441668.95207.1471856350880@...>';

update posts set parent='<lo12b9+1ctiud2@...>'
where id='<5FE431BE-3547-4D5A-BD2D-6391F311DA20@...>';

delete from post_lines where post in (select id from posts where parent='<20180719134536.72C7B20E5F@forge.ada-ru.org>');
delete from posts where parent='<20180719134536.72C7B20E5F@forge.ada-ru.org>';

delete from post_lines where post in ('<00003f5ecb78$22f05824$ab51f9db$@...>',
'<0000130847f8$dafe73ce$0ccb5713$@...>',
'<000026dd70ce$3fda9809$934e0821$@...>',
'<000085382bf1$eb390e04$0863ef5f$@...>',
'<977888565.2458006.1450664500190.JavaMail.yahoo@...>',
'<5677AC08.7080709@...>',
'<1951281157.2652477.1450693926839.JavaMail.yahoo@...>',
'<0000d0b79027$e69620b5$99172896$@...>',
'<1322035140.92855.YahooMailClassic@...>',
'<4ECCA8EE.5000303@...>',
'<4ECCB6B4.3070502@...>',
'<E1QwoDq-00048S-00.vadim_tukaev-bk-ru@...>',
'<p97s6r+evergj@YahooGroups.com>',
'<20180719134536.72C7B20E5F@forge.ada-ru.org>',
'<DB6PR0302MB2760132E1E659B75103B2F9EA7DB0@DB6PR0302MB2760.eurprd03.prod.outlook.com>',
'<DB6PR0302MB276046E89A99C3CDE0926558A7DB0@DB6PR0302MB2760.eurprd03.prod.outlook.com>',
'<546303381.59.1515783469010.JavaMail.byserg@liubou-home>'
);

delete from posts where id in (select id from posts
except select post from post_lines
except select parent from posts
);

update posts set parent='<51B5C6EA.50900@...>'
where id in ('<1375907734.27565.YahooMailNeo@...>',
'<1375916970.43532.YahooMailNeo@...>',
'<CANH59R4MHozYGVWQoDRfrF+f7CBR5EYbd2zECkMzhUnwfQ3Bcg@...>');
update posts set
parent='<CAO2-bK9nM8GAVNTXQKSUV5GdH1dDPYh2mbXMn_N=ga4byVdtwg@...>'
where id='<51AC5C6B.8090805@...>';
update posts set parent='<518FE59B.9080504@...>'
where id in ('<s5d4mv38cq7j$.tslosl68fd24$.dlg@...>',
'<1dz6ab74qmkwl$.rubmmkmezheu$.dlg@...>');
update posts set
parent='<d74d00cb8e39e7830c44e7f65462219c2635f9fd@...>'
where id='<51385553.5030404@...>';

update posts set parent='<51B5C6EA.50900@...>'
where id in ('<1375907734.27565.YahooMailNeo@...>',
'<1375916970.43532.YahooMailNeo@...>',
'<CANH59R4MHozYGVWQoDRfrF+f7CBR5EYbd2zECkMzhUnwfQ3Bcg@...>');
update posts set parent=''
where id in (
'<s5d4mv38cq7j$.tslosl68fd24$.dlg@...>',
'<1dz6ab74qmkwl$.rubmmkmezheu$.dlg@...>');
update posts set parent='<BD000697A1C64EBE9DF374F21FC27D5B@KIRKOROVSI>'
where id='<1t9zf9o68o248.iajhhk1irzo2$.dlg@...>';


delete from post_lines where (post, pos) in
(select p.post, p.pos from post_lines as p, (
select post, min(pos)  as pos from post_lines where
text like '----- Исходное сообщение -----%'and post like '%JavaMail.byserg%'
group by post) as x
where p.post = x.post and p.pos >= x.pos);

update posts set parent=null where id='<20200118160312.DBE872409E@forge.ada-ru.org>';
update posts set parent='<20200118160312.DBE872409E@forge.ada-ru.org>' where id='<20200119063236.4F2F3240AE@forge.ada-ru.org>';
