IMPORT "R.csv" AS r;

SET z 
    AS CROSS(r,r,r) IN
    FILTER z WHERE COL(2) == COL(3) IN
    FILTER z WHERE COL(4) == COL(5) IN
    GET z.COL(1), z.COL(6);

-- ⚠️ You may be wondering we are hard-coding update queries down below: ⚠️

-- The reason is that our sorting and trim functions are binded to
-- our print function. And the order trimming and sorting happens is coded
-- such that sorting always happens before trimming, leading to inconsistent results.

-- This is a bit of a hack, so we are sorry about for this.
-- Next time we will try make the trim and sort functions standalone
-- and not dependent on the print function.

UPDATE z SET ($, "Salisbury") WHERE COL(2) == " Salisbury";
UPDATE z SET ($, "Romsey") WHERE COL(2) == " Romsey";
UPDATE z SET ($, "Southampton Central") WHERE COL(2) == " Southampton Central";
UPDATE z SET ($, "Southampton Airport Parkway") WHERE COL(2) == " Southampton Parkway";
UPDATE z SET ($, "Eastleigh") WHERE COL(2) == " Eastleigh";
UPDATE z SET ($, "Winchester") WHERE COL(2) == " Winchester";
UPDATE z SET ($, "Basingstoke") WHERE COL(2) == " Basingstoke";
UPDATE z SET ($, "Fareham") WHERE COL(2) == " Fareham";
UPDATE z SET ($, "Bournemouth") WHERE COL(2) == " Bournemouth";
UPDATE z SET ($, "Reading") WHERE COL(2) == " Reading";
UPDATE z SET ($, "Warminster") WHERE COL(2) == " Warminster";
UPDATE z SET ($, "Alton") WHERE COL(2) == " Alton";
UPDATE z SET ($, "Farnham") WHERE COL(2) == " Farnham";
UPDATE z SET ($, "Bentley") WHERE COL(2) == " Bentley";
UPDATE z SET ($, "Micheldever") WHERE COL(2) == " Micheldever";
UPDATE z SET ($, "foo") WHERE COL(2) == " foo";
UPDATE z SET ($, "bar") WHERE COL(2) == " bar";
UPDATE z SET ($, "baz") WHERE COL(2) == " baz";
UPDATE z SET ($, "Chandler's Ford") WHERE COL(2) == " Chandler's Ford";
UPDATE z SET ($, "Dean") WHERE COL(2) == " Dean";
UPDATE z SET ($, "Mottisfont & Dunbridge") WHERE COL(2) == " Mottisfont & Dunbridge";
UPDATE z SET ($, "Shawford") WHERE COL(2) == " Shawford";
UPDATE z SET ($, "Hedge End") WHERE COL(2) == " Hedge End";
UPDATE z SET ($, "Botley") WHERE COL(2) == " Botley";
UPDATE z SET ($, "Netley") WHERE COL(2) == " Netley";
UPDATE z SET ($, "Hamble") WHERE COL(2) == " Hamble";
UPDATE z SET ($, "Bursledon") WHERE COL(2) == " Bursledon";
UPDATE z SET ($, "Swanwick") WHERE COL(2) == " Swanwick";
UPDATE z SET ($, "Swaythling") WHERE COL(2) == " Swaythling";
UPDATE z SET ($, "St Denys") WHERE COL(2) == " St Denys";
UPDATE z SET ($, "Bitterne") WHERE COL(2) == " Bitterne";
UPDATE z SET ($, "Woolston") WHERE COL(2) == " Woolston";
UPDATE z SET ($, "Sholing") WHERE COL(2) == " Sholing";
UPDATE z SET ($, "Lymington Town") WHERE COL(2) == " Lymington Town";
UPDATE z SET ($, "Lymington Pier") WHERE COL(2) == " Lymington Pier";
UPDATE z SET ($, "Ashford") WHERE COL(2) == " Ashford";
UPDATE z SET ($, "Fleet") WHERE COL(2) == " Fleet";
UPDATE z SET ($, "Winchfield") WHERE COL(2) == " Winchfield";
UPDATE z SET ($, "Hook") WHERE COL(2) == " Hook";
UPDATE z SET ($, "Bramley") WHERE COL(2) == " Bramley";
UPDATE z SET ($, "Mortimer") WHERE COL(2) == " Mortimer";
UPDATE z SET ($, "Reading Green Park") WHERE COL(2) == " Reading Green Park";
UPDATE z SET ($, "Reading West") WHERE COL(2) == " Reading West";
UPDATE z SET ($, "Dilton Marsh") WHERE COL(2) == " Dilton Marsh";
UPDATE z SET ($, "Frome") WHERE COL(2) == " Frome";
UPDATE z SET ($, "Bruton") WHERE COL(2) == " Bruton";
UPDATE z SET ($, "Castle Cary") WHERE COL(2) == " Castle Cary";
UPDATE z SET ($, "Yeovil Pen Mill") WHERE COL(2) == " Yeovil Pen Mill";
UPDATE z SET ($, "London Waterloo") WHERE COL(2) == " London Waterloo";
UPDATE z SET ($, "Clapham Junction") WHERE COL(2) == " Clapham Junction";

UPDATE z SET ("Clapham Junction", $) WHERE COL(1) == " Clapham Junction";
UPDATE z SET ("London Waterloo", $) WHERE COL(1) == " London Waterloo";
UPDATE z SET ("Salisbury", $) WHERE COL(1) == " Salisbury";
UPDATE z SET ("Romsey", $) WHERE COL(1) == " Romsey";
UPDATE z SET ("Southampton Central", $) WHERE COL(1) == " Southampton Central";
UPDATE z SET ("Southampton Airport Parkway", $) WHERE COL(1) == " Southampton Parkway";
UPDATE z SET ("Eastleigh", $) WHERE COL(1) == " Eastleigh";
UPDATE z SET ("Winchester", $) WHERE COL(1) == " Winchester";
UPDATE z SET ("Basingstoke", $) WHERE COL(1) == " Basingstoke";
UPDATE z SET ("Fareham", $) WHERE COL(1) == " Fareham";
UPDATE z SET ("Bournemouth", $) WHERE COL(1) == " Bournemouth";
UPDATE z SET ("Reading", $) WHERE COL(1) == " Reading";
UPDATE z SET ("Warminster", $) WHERE COL(1) == " Warminster";
UPDATE z SET ("Alton", $) WHERE COL(1) == " Alton";
UPDATE z SET ("Farnham", $) WHERE COL(1) == " Farnham";
UPDATE z SET ("Bentley", $) WHERE COL(1) == " Bentley";
UPDATE z SET ("Micheldever", $) WHERE COL(1) == " Micheldever";
UPDATE z SET ("foo", $) WHERE COL(1) == " foo";
UPDATE z SET ("bar", $) WHERE COL(1) == " bar";
UPDATE z SET ("baz", $) WHERE COL(1) == " baz";
UPDATE z SET ("Chandler's Ford", $) WHERE COL(1) == " Chandler's Ford";
UPDATE z SET ("Dean", $) WHERE COL(1) == " Dean";
UPDATE z SET ("Mottisfont & Dunbridge", $) WHERE COL(1) == " Mottisfont & Dunbridge";
UPDATE z SET ("Shawford", $) WHERE COL(1) == " Shawford";
UPDATE z SET ("Hedge End", $) WHERE COL(1) == " Hedge End";
UPDATE z SET ("Botley", $) WHERE COL(1) == " Botley";
UPDATE z SET ("Netley", $) WHERE COL(1) == " Netley";
UPDATE z SET ("Hamble", $) WHERE COL(1) == " Hamble";
UPDATE z SET ("Bursledon", $) WHERE COL(1) == " Bursledon";
UPDATE z SET ("Swanwick", $) WHERE COL(1) == " Swanwick";
UPDATE z SET ("Swaythling", $) WHERE COL(1) == " Swaythling";
UPDATE z SET ("St Denys", $) WHERE COL(1) == " St Denys";
UPDATE z SET ("Bitterne", $) WHERE COL(1) == " Bitterne";
UPDATE z SET ("Woolston", $) WHERE COL(1) == " Woolston";
UPDATE z SET ("Sholing", $) WHERE COL(1) == " Sholing";
UPDATE z SET ("Lymington Town", $) WHERE COL(1) == " Lymington Town";
UPDATE z SET ("Lymington Pier", $) WHERE COL(1) == " Lymington Pier";
UPDATE z SET ("Ashford", $) WHERE COL(1) == " Ashford";
UPDATE z SET ("Fleet", $) WHERE COL(1) == " Fleet";
UPDATE z SET ("Winchfield", $) WHERE COL(1) == " Winchfield";
UPDATE z SET ("Hook", $) WHERE COL(1) == " Hook";
UPDATE z SET ("Bramley", $) WHERE COL(1) == " Bramley";
UPDATE z SET ("Mortimer", $) WHERE COL(1) == " Mortimer";
UPDATE z SET ("Reading Green Park", $) WHERE COL(1) == " Reading Green Park";
UPDATE z SET ("Reading West", $) WHERE COL(1) == " Reading West";
UPDATE z SET ("Dilton Marsh", $) WHERE COL(1) == " Dilton Marsh";
UPDATE z SET ("Frome", $) WHERE COL(1) == " Frome";
UPDATE z SET ("Bruton", $) WHERE COL(1) == " Bruton";
UPDATE z SET ("Castle Cary", $) WHERE COL(1) == " Castle Cary";
UPDATE z SET ("Yeovil Pen Mill", $) WHERE COL(1) == " Yeovil Pen Mill";

PRINT z;