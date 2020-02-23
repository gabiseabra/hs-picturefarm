insert into tag_aliases values
-- mapping slack emoji to unicode
-- people and faces
  ('😁', ':grin:')
, ('😀', ':grinning:')
, ('😃', ':smiley:')
, ('😄', ':smile:')
, ('😆', ':laughing:')
, ('😆', ':satisfied:')
, ('😅', ':sweat_smile:')
, ('🤣', ':rolling_on_the_floor_laughing:')
, ('😂', ':joy:')
, ('🙂', ':slightly_smiling_face:')
, ('🙃', ':upside_down_face:')
, ('😉', ':wink:')
, ('😊', ':blush:')
, ('😇', ':innocent:')
, ('😍', ':heart_eyes:')
, ('🤩', ':star-struck:')
, ('😘', ':kissing_heart:')
, ('😗', ':kissing:')
, ('☺', ':relaxed:')
, ('😚', ':kissing_closed_eyes:')
, ('😙', ':kissing_smiling_eyes:')
, ('😋', ':yum:')
, ('😛', ':stuck_out_tongue:')
, ('😜', ':stuck_out_tongue_winking_eye:')
, ('🤪', ':zany_face:')
, ('😝', ':stuck_out_tongue_closed_eyes:')
, ('🤑', ':money_mouth_face:')
, ('🤗', ':hugging_face:')
, ('🤭', ':face_with_hand_over_mouth:')
, ('🤫', ':shushing_face:')
, ('🤔', ':thinking_face:')
, ('🤐', ':zipper_mouth_face:')
, ('🤨', ':face_with_raised_eyebrow:')
, ('😐', ':neutral_face:')
, ('😑', ':expressionless:')
, ('😶', ':no_mouth:')
, ('😏', ':smirk:')
, ('😒', ':unamused:')
, ('🙄', ':face_with_rolling_eyes:')
, ('😬', ':grimacing:')
, ('🤥', ':lying_face:')
, ('😌', ':relieved:')
, ('😔', ':pensive:')
, ('😪', ':sleepy:')
, ('🤤', ':drooling_face:')
, ('😴', ':sleeping:')
, ('😷', ':mask:')
, ('🤒', ':face_with_thermometer:')
, ('🤕', ':face_with_head_bandage:')
, ('🤢', ':nauseated_face:')
, ('🤮', ':face_vomiting:')
, ('🤧', ':sneezing_face:')
-- , ('🥵', '')
-- , ('🥶', '')
-- , ('🥴', '')
, ('😵', ':dizzy_face:')
, ('🤯', ':exploding_head:')
, ('🤠', ':face_with_cowboy_hat:')
-- , ('🥳', '')
, ('😎', ':sunglasses:')
, ('🤓', ':nerd_face:')
, ('🧐', ':face_with_monocle:')
, ('😕', ':confused:')
, ('😟', ':worried:')
, ('🙁', ':slightly_frowning_face:')
, ('☹', ':white_frowning_face:')
, ('😮', ':open_mouth:')
, ('😯', ':hushed:')
, ('😲', ':astonished:')
, ('😳', ':flushed:')
-- , ('🥺', '')
, ('😦', ':frowning:')
, ('😧', ':anguished:')
, ('😨', ':fearful:')
, ('😰', ':cold_sweat:')
-- , ('😥', '')
, ('😢', ':cry:')
, ('😭', ':sob:')
, ('😱', ':scream:')
, ('😖', ':confounded:')
, ('😣', ':persevere:')
, ('😞', ':disappointed:')
, ('😓', ':sweat:')
, ('😩', ':weary:')
, ('😫', ':tired_face:')
, ('😤', ':triumph:')
, ('😡', ':rage:')
, ('😠', ':angry:')
, ('🤬', ':face_with_symbols_on_mouth:')
, ('😈', ':smiling_imp:')
, ('👿', ':imp:')
, ('💩', ':poop:')
, ('🤡', ':clown_face:')
, ('👽', ':alien:')
, ('👾', ':space_invader:')
, ('🤖', ':robot_face:')
, ('😼', ':smirk_cat:')
-- aliases
, ('😚', '😙')
, ('🙁', '☹')
, ('😮', '😯')
, ('😮', '😲')
on conflict (tag, alias) do nothing;
