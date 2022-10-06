#* LoR-Meta Tier List
#* @get /player/<region>/<name>
function(region = '', name = '') {
  
  tag <- lorr::get_db_query(
    "
    SELECT
      tagLine
    FROM
      utils_players
    WHERE
      region = '{region}'
      AND gameName = '{URLdecode(name)}'
  "
  )[[1]]
  
    
  return(tag)

}
