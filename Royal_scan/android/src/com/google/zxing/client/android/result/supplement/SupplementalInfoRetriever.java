/*
 * Copyright (C) 2010 ZXing authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.zxing.client.android.result.supplement;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Handler;
import android.text.Html;
import android.text.Spanned;
import android.view.View;
import android.widget.TextView;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.concurrent.Callable;

public abstract class SupplementalInfoRetriever implements Callable<Void> {

  private final WeakReference<TextView> textViewRef;
  private final Handler handler;
  private final Context context;

  SupplementalInfoRetriever(TextView textView, Handler handler, Context context) {
    this.textViewRef = new WeakReference<TextView>(textView);
    this.handler = handler;
    this.context = context;
  }

  public final Void call() throws IOException, InterruptedException {
    retrieveSupplementalInfo();
    return null;
  }

  abstract void retrieveSupplementalInfo() throws IOException, InterruptedException;

  final void append(final String newText) throws InterruptedException {
    final TextView textView = textViewRef.get();
    if (textView == null) {
      throw new InterruptedException();
    }
    handler.post(new Runnable() {
      public void run() {
        Spanned html = Html.fromHtml(newText + '\n');
        textView.append(html);
      }
    });
  }

  final void setLink(final String uri) {
    textViewRef.get().setOnClickListener(new View.OnClickListener() {
      public void onClick(View view) {
        Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(uri));
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
        context.startActivity(intent);
      }
    });
  }

}
