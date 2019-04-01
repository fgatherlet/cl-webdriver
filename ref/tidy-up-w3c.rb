COMMANDS = {
  # session handling
  new_session:    [:pst, '../../session'], #### this is specific
  delete_session: [:del, ''],

  # basic driver
  get_current_url: [:get, '/url'],
  forward:         [:pst, '/forward'],
  back:            [:pst, '/back'],
  refresh:         [:pst, '/refresh'],
  get_page_source: [:get, '/source'],
  get_title:       [:get, '/title'],

  # document handling
  execute_script:       [:pst, '/execute/sync'],
  execute_async_script: [:pst, '/execute/async'],

  # screenshot
  take_screenshot:         [:get, '/screenshot'],
  take_element_screenshot: [:get, '/element/:id/screenshot'],

  # target location
  switch_to_frame:        [:pst, '/frame'],
  switch_to_parent_frame: [:pst, '/frame/parent'],

  # cookies
  get_all_cookies:    [:get, '/cookie'],
  get_cookie:         [:get, '/cookie/:name'],
  add_cookie:         [:pst, '/cookie'],
  delete_cookie:      [:del, '/cookie/:name'],
  delete_all_cookies: [:del, '/cookie'],

  # window and Frame handling
  get_window_handle:   [:get, '/window'],
  close_window:        [:del, '/window'],
  switch_to_window:    [:pst, '/window'],
  get_window_handles:  [:get, '/window/handles'],
  fullscreen_window:   [:pst, '/window/fullscreen'],
  minimize_window:     [:pst, '/window/minimize'],
  maximize_window:     [:pst, '/window/maximize'],
  set_window_size:     [:pst, '/window/size'],
  get_window_size:     [:get, '/window/size'],
  set_window_position: [:pst, '/window/position'],
  get_window_position: [:get, '/window/position'],
  set_window_rect:     [:pst, '/window/rect'],
  get_window_rect:     [:get, '/window/rect'],

  # element
  find_element:          [:pst, '/element'],
  find_elements:         [:pst, '/elements'],
  find_child_element:    [:pst, '/element/:id/element'],
  find_child_elements:   [:pst, '/element/:id/elements'],
  get_active_element:    [:get, '/element/active'],
  is_element_selected:   [:get, '/element/:id/selected'],
  get_element_attribute: [:get, '/element/:id/attribute/:name'],
  get_element_property:  [:get, '/element/:id/property/:name'],
  get_element_css_value: [:get, '/element/:id/css/:property_name'],
  get_element_text:      [:get, '/element/:id/text'],
  get_element_tag_name:  [:get, '/element/:id/name'],
  get_element_rect:      [:get, '/element/:id/rect'],
  is_element_enabled:    [:get, '/element/:id/enabled'],
  # Element Operations
  element_click:     [:pst, '/element/:id/click'],
  element_tap:       [:pst, '/element/:id/tap'],
  element_clear:     [:pst, '/element/:id/clear'],
  element_send_keys: [:pst, '/element/:id/value'],

  # timeouts
  set_timeout: [:pst, '/timeouts'],

  # actions
  actions:         [:pst, '/actions'],
  release_actions: [:del, '/actions'],

  # alerts
  dismiss_alert:   [:pst, '/alert/dismiss'],
  accept_alert:    [:pst, '/alert/accept'],
  get_alert_text:  [:get, '/alert/text'],
  send_alert_text: [:pst, '/alert/text'],
  # server extensions
  upload_file: [:pst, '/se/file']
}
