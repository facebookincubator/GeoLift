/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';


const features = [
  {
    title: 'Omni-Channel Efficiency with Hudsons Bay Company',
    imageUrl: 'https://upload.wikimedia.org/wikipedia/commons/0/04/Hudson%27s_Bay_Company_Official_Logo_2013.svg',
    description: (
      <>
        Using GeoLift, Hudson’s Bay was able to measure lift in online and in-store sales
        as a result of campaigns on Facebook and Instagram. The company saw a 12.9X incremental
        return on ad spend (iROAS) utilizing this approach.
        <a href="https://www.facebook.com/business/measurement/case-studies/hudsons-bay-company#">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: 'Omni-Channel Measurement for Scotiabank',
    imageUrl: 'https://i.pinimg.com/originals/3f/9c/83/3f9c8390a12bf2b96c7055169e77f333.png',
    description: (
      <>
        Scotiabank Mexico was able to measure for the first time the omni-channel
        incremental impact that their Facebook campaigns had on bank account openings.
        <a href="https://www.facebook.com/business/success/2-scotiabank-mexico?locale=es_LA">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: 'Building Brand and Sales with Super Coffee',
    imageUrl: 'https://scontent-sjc3-1.xx.fbcdn.net/v/t1.6435-9/146475455_3695451970571166_7309961661612546269_n.png?_nc_cat=105&_nc_map=test-rt&ccb=1-5&_nc_sid=09cbfe&_nc_eui2=AeG9H3ydn7q54QvS6IsuGgB7NEmGhia1Teo0SYaGJrVN6oOiRbUV_8TDSBfl_ngJ_Bk&_nc_ohc=q4vTJ5rfyBMAX9pNwle&_nc_ht=scontent-sjc3-1.xx&oh=00_AT8L-G0nU88JjThwNs0z-_C4Zk1O0wyi1sYGaJsDh8_8Hg&oe=626C79D2',
    description: (
      <>
        The enhanced coffee company ran Facebook video and photo ads to boost brand
        awareness and used a GeoLift study to measure in-store sales, revealing an
        11.9% in-store sales lift.
        <a href="https://www.facebook.com/business/success/super-coffee">
        -  Learn More.
        </a>
      </>
    ),
  },
  {
    title: 'Cross-Channel Measurement with Liverpool',
    imageUrl: 'https://scontent-sjc3-1.xx.fbcdn.net/v/t39.30808-6/275845667_10159868290766217_6878811270235505956_n.jpg?_nc_cat=1&_nc_map=test-rt&ccb=1-5&_nc_sid=09cbfe&_nc_eui2=AeEgXXkEQuNFEdIUcBHE_aXghNVcZa5P5DiE1Vxlrk_kODba9joGyL7vyoQ1PHdBONo&_nc_ohc=W6ae1Sr4zTYAX-8ZdVv&_nc_ht=scontent-sjc3-1.xx&oh=00_AT_U19NyRAUHoaK97Kbddy8R8wcjFQTNYN7RIvCxp1OP_g&oe=624C4405',
    description: (
      <>
        The mexican retailer measured the first cross-channel GeoLift  test comparing the
        omni-channel incrementality of Meta and Digital Display. The results revealed the
        importance of a robust cross-channel strategy with a 14X incremental ROAS for Meta.
        <a href="https://www.facebook.com/business/success/3-liverpool?locale=es_LA">
        -  Learn More.
        </a>
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

export default function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">{"GeoLift Success Cases"}</h1>
          <div className={styles.buttons}>
          </div>
        </div>
      </header>
      <main>
        {features && features.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}
