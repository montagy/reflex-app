module Views.Post (
  page
) where


import Reflex
import Reflex.Dom

page :: MonadWidget t m => m (Event t String)
page =
  elClass "div" "main" $ do
    postBlogV
    return never

postBlogV :: MonadWidget t m => m ()
postBlogV =
  el "form" $ do
    eTitle <- textInput def
    eContent <- textArea def
    _ <- button "Submit"
    pure ()
