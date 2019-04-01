
COMMANDS = {
  # session
  new_session:      [:pst, '../../session'], #### from w3c. this is specific!
  quit:             [:del, ''], # delete_session
  #### oss only
  status:           [:get, '../../status'],
  get_capabilities: [:get, ''], # oss only

  # basic driver
  get_current_url: [:get, '/url'],
  get:             [:pst, '/url'],
  go_forward:      [:pst, '/forward'],
  go_back:         [:pst, '/back'],
  refresh:         [:pst, '/refresh'],
  get_page_source: [:get, '/source'],
  get_title:       [:get, '/title'],

  # script execution
  execute_script:       [:pst, '/execute/sync'],
  execute_async_script: [:pst, '/execute_async'],
  #### oss only
  execute_script:       [:pst, '/execute'],

  # screenshot
  screenshot:              [:get, '/screenshot'],
  take_element_screenshot: [:get, '/element/:id/screenshot'],

  # target locator
  switch_to_frame:        [:pst, '/frame'],
  switch_to_parent_frame: [:pst, '/frame/parent'],

  # window handling (based on w3c)
  get_window_handle:         [:get, '/window'],
  close:                     [:del, '/window'],
  switch_to_window:          [:pst, '/window'], ## request: {"name": <window-id>}

  get_window_handles:        [:get, '/window/handles'],
  fullscreen_window:         [:pst, '/window/fullscreen'],
  minimize_window:           [:pst, '/window/minimize'],
  maximize_window:           [:pst, '/window/maximize'],
  set_window_size:           [:pst, '/window/size'],
  get_window_size:           [:get, '/window/size'],
  set_window_position:       [:pst, '/window/position'],
  get_window_position:       [:get, '/window/position'],
  set_window_rect:           [:pst, '/window/rect'],
  get_window_rect:           [:get, '/window/rect'],
  ####### oss only
  get_current_window_handle: [:get, '/window_handle'],
  get_window_handles:        [:get, '/window_handles'],
  get_window_size:           [:get, '/window/:window_handle/size'],
  set_window_size:           [:pst, '/window/:window_handle/size'],
  get_window_position:       [:get, '/window/:window_handle/position'],
  set_window_position:       [:pst, '/window/:window_handle/position'],
  maximize_window:           [:pst, '/window/:window_handle/maximize'],

  # element
  find_element:          [:pst, '/element'],
  find_elements:         [:pst, '/elements'],
  find_child_element:    [:pst, '/element/:id/element'],
  find_child_elements:   [:pst, '/element/:id/elements'],
  get_active_element:    [:pst, '/element/active'],
  is_element_selected:   [:get, '/element/:id/selected'], ## checkbox radio.
  get_element_attribute: [:get, '/element/:id/attribute/:name'],
  get_element_property:  [:get, '/element/:id/property/:name'],
  get_element_css_value: [:get, '/element/:id/css/:property_name'],
  get_element_text:      [:get, '/element/:id/text'],
  get_element_tag_name:  [:get, '/element/:id/name'],
  get_element_rect:      [:get, '/element/:id/rect'],
  is_element_enabled:    [:get, '/element/:id/enabled'],
  #### oss only
  get_element_value:     [:get, '/element/:id/value'],
  is_element_displayed:  [:get, '/element/:id/displayed'],
  get_element_location:  [:get, '/element/:id/location'],
  get_element_location_once_scrolled_into_view: [:get, '/element/:id/location_in_view'],
  get_element_size:      [:get, '/element/:id/size'],
  ### element_equals: [:get, '/element/:id/equals/:other'], ## needp
  ### submit_element: [:pst, '/element/:id/submit'], ## not supported?
  ### upload_file: [:pst, '/file'], ## ?? needp
  ### describe_element: [:get, '/element/:id'], ## not supported?
  ### drag_element: [:pst, '/element/:id/drag'], ## needp

  # element operations
  click_element:         [:pst, '/element/:id/click'],
  element_tap:           [:pst, '/element/:id/tap'],
  clear_element:         [:pst, '/element/:id/clear'],
  send_keys_to_element:  [:pst, '/element/:id/value'],

  # cookie
  get_cookies:        [:get, '/cookie'],
  add_cookie:         [:pst, '/cookie'],
  delete_all_cookies: [:del, '/cookie'],
  get_cookie:         [:get, '/cookie/:name'],
  delete_cookie:      [:del, '/cookie/:name'],

  # actions (w3c)
  actions:         [:pst, '/actions'],
  release_actions: [:del, '/actions'],
  ##### oss only
  click:                               [:pst, '/click'],
  double_click:                        [:pst, '/doubleclick'],
  mouse_down:                          [:pst, '/buttondown'],
  mouse_up:                            [:pst, '/buttonup'],
  mouse_move_to:                       [:pst, '/moveto'],
  send_modifier_key_to_active_element: [:pst, '/modifier'],
  send_keys_to_active_element:         [:pst, '/keys'],

  # alerts
  dismiss_alert:   [:pst, '/alert/dismiss'],
  accept_alert:    [:pst, '/alert/accept'],
  get_alert_text:  [:get, '/alert/text'],
  send_alert_text: [:pst, '/alert/text'],
  ##### oss only
  dismiss_alert:   [:pst, '/dismiss_alert'],
  accept_alert:    [:pst, '/accept_alert'],
  get_alert_text:  [:get, '/alert_text'],
  set_alert_value: [:pst, '/alert_text'],
  set_authentication: [:pst, '/alert/credentials'],

  # timeouts
  set_timeout:        [:pst, '/timeouts'],
  #### oss only
  implicitly_wait:    [:pst, '/timeouts/implicit_wait'],
  set_script_timeout: [:pst, '/timeouts/async_script'],

  ######## below is oss only

  # logs
  get_available_log_types: [:get, '/log/types'],
  get_log: [:pst, '/log']

  # html 5
  execute_sql: [:pst, '/execute_sql'],
  get_location: [:get, '/location'],
  set_location: [:pst, '/location'],
  get_app_cache: [:get, '/application_cache'],
  get_app_cache_status: [:get, '/application_cache/status'],
  clear_app_cache: [:del, '/application_cache/clear'],
  get_network_connection: [:get, '/network_connection'],
  set_network_connection: [:pst, '/network_connection'],
  get_local_storage_item: [:get, '/local_storage/key/:key'],
  remove_local_storage_item: [:del, '/local_storage/key/:key'],
  get_local_storage_keys: [:get, '/local_storage'],
  set_local_storage_item: [:pst, '/local_storage'],
  clear_local_storage: [:del, '/local_storage'],
  get_local_storage_size: [:get, '/local_storage/size'],
  get_session_storage_item: [:get, '/session_storage/key/:key'],
  remove_session_storage_item: [:del, '/session_storage/key/:key'],
  get_session_storage_keys: [:get, '/session_storage'],
  set_session_storage_item: [:pst, '/session_storage'],
  clear_session_storage: [:del, '/session_storage'],
  get_session_storage_size: [:get, '/session_storage/size'],

  # rotatable
  get_screen_orientation: [:get, '/orientation'],
  set_screen_orientation: [:pst, '/orientation'],
  # ime
  ime_get_available_engines: [:get, '/ime/available_engines'],
  ime_get_active_engine: [:get, '/ime/active_engine'],
  ime_is_activated: [:get, '/ime/activated'],
  ime_deactivate: [:pst, '/ime/deactivate'],
  ime_activate_engine: [:pst, '/ime/activate'],
  # touch
  touch_single_tap: [:pst, '/touch/click'],
  touch_double_tap: [:pst, '/touch/doubleclick'],
  touch_long_press: [:pst, '/touch/longclick'],
  touch_down: [:pst, '/touch/down'],
  touch_up: [:pst, '/touch/up'],
  touch_move: [:pst, '/touch/move'],
  touch_scroll: [:pst, '/touch/scroll'],
  touch_flick: [:pst, '/touch/flick'],
}
